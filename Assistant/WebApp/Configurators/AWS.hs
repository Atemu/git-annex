{- git-annex assistant webapp configurators for Amazon AWS services
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.AWS where

import Assistant.WebApp.Common
import Assistant.WebApp.MakeRemote
import qualified Remote.S3 as S3
import Logs.Remote
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.Glacier as Glacier
import qualified Remote.Helper.AWS as AWS
import Types.Remote (RemoteConfig)
import Types.StandardGroups
import Creds
import Assistant.Gpg
import Git.Types (RemoteName)
import Annex.SpecialRemote.Config
import Types.ProposedAccepted

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char

awsConfigurator :: Widget -> Handler Html
awsConfigurator = page "Add an Amazon repository" (Just Configuration)

glacierConfigurator :: Widget -> Handler Html
glacierConfigurator a = do
	ifM (liftIO $ inSearchPath "glacier")
		( awsConfigurator a
		, awsConfigurator needglaciercli
		)
  where
	needglaciercli = $(widgetFile "configurators/needglaciercli")

data StorageClass
	= StandardRedundancy
	| StandardInfrequentAccess
	deriving (Eq, Enum, Bounded)

instance Show StorageClass where
	show StandardRedundancy = "STANDARD" 
	show StandardInfrequentAccess = "STANDARD_IA"

data AWSInput = AWSInput
	{ accessKeyID :: Text
	, secretAccessKey :: Text
	, datacenter :: Text
	-- Only used for S3, not Glacier.
	, storageClass :: StorageClass
	, repoName :: Text
	, enableEncryption :: EnableEncryption
	}

data AWSCreds = AWSCreds Text Text

extractCreds :: AWSInput -> AWSCreds
extractCreds i = AWSCreds (accessKeyID i) (secretAccessKey i)

s3InputAForm :: Maybe CredPair -> MkAForm AWSInput
s3InputAForm defcreds = AWSInput
	<$> accessKeyIDFieldWithHelp (T.pack . fst <$> defcreds)
	<*> secretAccessKeyField (T.pack . snd <$> defcreds)
	<*> datacenterField AWS.S3
	<*> areq (selectFieldList storageclasses) (bfs "Storage class") (Just StandardRedundancy)
	<*> areq textField (bfs "Repository name") (Just "S3")
	<*> enableEncryptionField
  where
	storageclasses :: [(Text, StorageClass)]
	storageclasses =
		[ ("Standard redundancy", StandardRedundancy)
		, ("Infrequent access (cheaper for backups and archives)", StandardInfrequentAccess)
		]

glacierInputAForm :: Maybe CredPair -> MkAForm AWSInput
glacierInputAForm defcreds = AWSInput
	<$> accessKeyIDFieldWithHelp (T.pack . fst <$> defcreds)
	<*> secretAccessKeyField (T.pack . snd <$> defcreds)
	<*> datacenterField AWS.Glacier
	<*> pure StandardRedundancy
	<*> areq textField (bfs "Repository name") (Just "glacier")
	<*> enableEncryptionField

awsCredsAForm :: Maybe CredPair -> MkAForm AWSCreds
awsCredsAForm defcreds = AWSCreds
	<$> accessKeyIDFieldWithHelp (T.pack . fst <$> defcreds)
	<*> secretAccessKeyField (T.pack . snd <$> defcreds)

accessKeyIDField :: Widget -> Maybe Text -> MkAForm Text
accessKeyIDField help = areq (textField `withNote` help) (bfs "Access Key ID")

accessKeyIDFieldWithHelp :: Maybe Text -> MkAForm Text
accessKeyIDFieldWithHelp = accessKeyIDField help
  where
	help = [whamlet|
<a href="https://portal.aws.amazon.com/gp/aws/securityCredentials#id_block">
  Get Amazon access keys
|]

secretAccessKeyField :: Maybe Text -> MkAForm Text
secretAccessKeyField = areq passwordField (bfs "Secret Access Key")

datacenterField :: AWS.Service -> MkAForm Text
datacenterField service = areq (selectFieldList list) (bfs "Datacenter") defregion
  where
	list = M.toList $ AWS.regionMap service
	defregion = Just $ AWS.defaultRegion service

getAddS3R :: Handler Html
getAddS3R = postAddS3R

postAddS3R :: Handler Html
postAddS3R = awsConfigurator $ do
	defcreds <- liftAnnex previouslyUsedAWSCreds
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ s3InputAForm defcreds
	case result of
		FormSuccess input -> liftH $ do
			let name = T.unpack $ repoName input
			makeAWSRemote initSpecialRemote S3.remote TransferGroup (extractCreds input) name $ M.fromList
				[ configureEncryption $ enableEncryption input
				, (typeField, Proposed "S3")
				, (Proposed "datacenter", Proposed $ T.unpack $ datacenter input)
				, (Proposed "storageclass", Proposed $ show $ storageClass input)
				, (Proposed "chunk", Proposed "1MiB")
				]
		_ -> $(widgetFile "configurators/adds3")

getAddGlacierR :: Handler Html
getAddGlacierR = postAddGlacierR

postAddGlacierR :: Handler Html
postAddGlacierR = glacierConfigurator $ do
	defcreds <- liftAnnex previouslyUsedAWSCreds
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ glacierInputAForm defcreds
	case result of
		FormSuccess input -> liftH $ do
			let name = T.unpack $ repoName input
			makeAWSRemote initSpecialRemote Glacier.remote SmallArchiveGroup (extractCreds input) name $ M.fromList
				[ configureEncryption $ enableEncryption input
				, (typeField, Proposed "glacier")
				, (Proposed "datacenter", Proposed $ T.unpack $ datacenter input)
				]
		_ -> $(widgetFile "configurators/addglacier")

getEnableS3R :: UUID -> Handler Html
getEnableS3R uuid = do
	m <- liftAnnex remoteConfigMap
	isia <- case M.lookup uuid m of
		Just c -> liftAnnex $ do
			pc <- parsedRemoteConfig S3.remote c
			return $ S3.configIA pc
		Nothing -> return False
	if isia
		then redirect $ EnableIAR uuid
		else postEnableS3R uuid

postEnableS3R :: UUID -> Handler Html
postEnableS3R uuid = awsConfigurator $ enableAWSRemote S3.remote uuid

getEnableGlacierR :: UUID -> Handler Html
getEnableGlacierR = postEnableGlacierR

postEnableGlacierR :: UUID -> Handler Html
postEnableGlacierR = glacierConfigurator . enableAWSRemote Glacier.remote

enableAWSRemote :: RemoteType -> UUID -> Widget
enableAWSRemote remotetype uuid = do
	defcreds <- liftAnnex previouslyUsedAWSCreds
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ awsCredsAForm defcreds
	case result of
		FormSuccess creds -> liftH $ do
			m <- liftAnnex remoteConfigMap
			let name = fromJust $ lookupName $
				fromJust $ M.lookup uuid m
			makeAWSRemote enableSpecialRemote remotetype SmallArchiveGroup creds name M.empty
		_ -> do
			description <- liftAnnex $
				T.pack <$> Remote.prettyUUID uuid
			$(widgetFile "configurators/enableaws")

makeAWSRemote :: SpecialRemoteMaker -> RemoteType -> StandardGroup -> AWSCreds -> RemoteName -> RemoteConfig -> Handler ()
makeAWSRemote maker remotetype defaultgroup (AWSCreds ak sk) name config = 
	setupCloudRemote defaultgroup Nothing $
		maker hostname remotetype (Just creds) config
  where
	creds = (T.unpack ak, T.unpack sk)
	{- AWS services use the remote name as the basis for a host
	 - name, so filter it to contain valid characters. -}
	hostname = case filter isAlphaNum name of
		[] -> "aws"
		n -> n

getRepoInfo :: RemoteConfig -> Widget
getRepoInfo c = [whamlet|S3 remote using bucket: #{bucket}|]
  where
	bucket = maybe "" fromProposedAccepted $ M.lookup (Accepted "bucket") c

previouslyUsedAWSCreds :: Annex (Maybe CredPair)
previouslyUsedAWSCreds = getM gettype [S3.remote, Glacier.remote]
  where
	gettype t = previouslyUsedCredPair AWS.creds t $
		not . S3.configIA . Remote.config
