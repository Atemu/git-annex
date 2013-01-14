{- git-annex assistant webapp configurators for Amazon AWS services
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.AWS where

import Assistant.WebApp.Common
import Assistant.MakeRemote
import Assistant.Sync
#ifdef WITH_S3
import qualified Remote.S3 as S3
#endif
import qualified Remote.Glacier as Glacier
import qualified Remote.Helper.AWS as AWS
import Logs.Remote
import qualified Remote
import Types.Remote (RemoteConfig)
import Types.StandardGroups
import Logs.PreferredContent

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char

awsConfigurator :: Widget -> Handler RepHtml
awsConfigurator = page "Add an Amazon repository" (Just Configuration)

glacierConfigurator :: Widget -> Handler RepHtml
glacierConfigurator a = do
	ifM (liftIO $ inPath "glacier")
		( awsConfigurator a
		, awsConfigurator needglaciercli
		)
  where
	needglaciercli = $(widgetFile "configurators/needglaciercli")

data StorageClass = StandardRedundancy | ReducedRedundancy
	deriving (Eq, Enum, Bounded)

instance Show StorageClass where
	show StandardRedundancy = "STANDARD" 
	show ReducedRedundancy = "REDUCED_REDUNDANCY"

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

s3InputAForm :: AForm WebApp WebApp AWSInput
s3InputAForm = AWSInput
	<$> accessKeyIDField
	<*> secretAccessKeyField
	<*> datacenterField AWS.S3
	<*> areq (selectFieldList storageclasses) "Storage class" (Just StandardRedundancy)
	<*> areq textField "Repository name" (Just "S3")
	<*> enableEncryptionField
  where
	storageclasses :: [(Text, StorageClass)]
	storageclasses =
		[ ("Standard redundancy", StandardRedundancy)
		, ("Reduced redundancy (costs less)", ReducedRedundancy)
		]

glacierInputAForm :: AForm WebApp WebApp AWSInput
glacierInputAForm = AWSInput
	<$> accessKeyIDField
	<*> secretAccessKeyField
	<*> datacenterField AWS.Glacier
	<*> pure StandardRedundancy
	<*> areq textField "Repository name" (Just "glacier")
	<*> enableEncryptionField

awsCredsAForm :: AForm WebApp WebApp AWSCreds
awsCredsAForm = AWSCreds
	<$> accessKeyIDField
	<*> secretAccessKeyField

accessKeyIDField :: AForm WebApp WebApp Text
accessKeyIDField = areq (textField `withNote` help) "Access Key ID" Nothing
  where
	help = [whamlet|
<a href="https://portal.aws.amazon.com/gp/aws/securityCredentials#id_block">
  Get Amazon access keys
|]

secretAccessKeyField :: AForm WebApp WebApp Text
secretAccessKeyField = areq passwordField "Secret Access Key" Nothing

datacenterField :: AWS.Service -> AForm WebApp WebApp Text
datacenterField service = areq (selectFieldList list) "Datacenter" defregion
  where
	list = M.toList $ AWS.regionMap service
	defregion = Just $ AWS.defaultRegion service

getAddS3R :: Handler RepHtml
#ifdef WITH_S3
getAddS3R = awsConfigurator $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap s3InputAForm
	case result of
		FormSuccess input -> lift $ do
			let name = T.unpack $ repoName input
			makeAWSRemote S3.remote (extractCreds input) name setgroup $ M.fromList
				[ configureEncryption $ enableEncryption input
				, ("type", "S3")
				, ("datacenter", T.unpack $ datacenter input)
				, ("storageclass", show $ storageClass input)
				]
		_ -> $(widgetFile "configurators/adds3")
  where
	setgroup r = runAnnex () $
		setStandardGroup (Remote.uuid r) TransferGroup
#else
getAddS3R = error "S3 not supported by this build"
#endif

getAddGlacierR :: Handler RepHtml
getAddGlacierR = glacierConfigurator $ do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap glacierInputAForm
	case result of
		FormSuccess input -> lift $ do
			let name = T.unpack $ repoName input
			makeAWSRemote Glacier.remote (extractCreds input) name setgroup $ M.fromList
				[ configureEncryption $ enableEncryption input
				, ("type", "glacier")
				, ("datacenter", T.unpack $ datacenter input)
				]
		_ -> $(widgetFile "configurators/addglacier")
  where
	setgroup r = runAnnex () $
		setStandardGroup (Remote.uuid r) SmallArchiveGroup

getEnableS3R :: UUID -> Handler RepHtml
#ifdef WITH_S3
getEnableS3R = awsConfigurator . enableAWSRemote S3.remote
#else
getEnableS3R _ = error "S3 not supported by this build"
#endif

getEnableGlacierR :: UUID -> Handler RepHtml
getEnableGlacierR = glacierConfigurator . enableAWSRemote Glacier.remote

enableAWSRemote :: RemoteType -> UUID -> Widget
enableAWSRemote remotetype uuid = do
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap awsCredsAForm
	case result of
		FormSuccess creds -> lift $ do
			m <- runAnnex M.empty readRemoteLog
			let name = fromJust $ M.lookup "name" $
				fromJust $ M.lookup uuid m
			makeAWSRemote remotetype creds name (const noop) M.empty
		_ -> do
			description <- lift $ runAnnex "" $
				T.pack . concat <$> Remote.prettyListUUIDs [uuid]
			$(widgetFile "configurators/enableaws")

makeAWSRemote :: RemoteType -> AWSCreds -> String -> (Remote -> Handler ()) -> RemoteConfig -> Handler ()
makeAWSRemote remotetype (AWSCreds ak sk) name setup config = do
	remotename <- runAnnex name $ fromRepo $ uniqueRemoteName name 0
	liftIO $ AWS.setCredsEnv (T.unpack ak, T.unpack sk)
	r <- liftAssistant $ liftAnnex $ addRemote $ do
		makeSpecialRemote hostname remotetype config
		return remotename
	setup r
	liftAssistant $ syncNewRemote r
	redirect $ EditNewCloudRepositoryR $ Remote.uuid r
  where
	{- AWS services use the remote name as the basis for a host
	 - name, so filter it to contain valid characters. -}
	hostname = case filter isAlphaNum name of
		[] -> "aws"
		n -> n
