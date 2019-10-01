module MusicResources where

import System.Random
import System.IO.Unsafe

-- randomly chosen from the range [0 .. x]            
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

chars = "0123456789abcdefghijklmnopqrstuvwxyzBCDEGHIIJOPQSTVWYZ"

furElise = "fDfDfadsptupauOasufDfDfadsptupausapasdfogfdifdsudsaufuffxDfDfDfDfDfDfadsptupauOasufDfDfadsptupausap"
moonLightSonata = "9eyieyieyieyi8eyieyieyieyiEyiEyiEYoEYo6eToeyi6eyuwTu9qeyeyieyipeyppuoeuoeuopuop9pyieyiPyoEyo8ptieti8otusuo4itietietietiWtiWtiWtiOtiOOtIOtIOtIOtIOTiWTiWyioyi"
secondWaltz = "fsappaspasfgfddapOOpaOpadgDflkjhgdkjhhfsdffdfgdsfdssfh"
preludeInCMajor = "tuosfosftuosfosftypdgpdgtypdgpdgryodgodgryodgodgtuosfosftuosfosftupfjpfjtupfjpfjtyIpdIpdtyIpdIpdryodhodhryodhodhrtuosuosrtuosuosetuosuosetuosuos9eyIsyIs9eyIsyIswryoayoawryoayoawEuoSuoSwEuoSuoSqeypdypdqeypdypdqwyiaqwyia0wtostos0wtostos0qetieti0qetieti9qetieti9qetieti59wriwri59wriwri80wtuwtu80wtuwtu8wetuetu8wetuetu4qetuetu4qetuetu48etyety48etyety5qrtyrty5qrtyrty5qwrywry5qwrywry50wtuwtu50wtuwtu59wtiwti59wtiwti59wriwri59wriwri59etieti59etieti50wtowto50wto59wtiwti59wtiwti59wriwri59wri18weuweu18weuweu18qetiteteqe9q917oadgdadaoayiuy18uos"
caspersLullaby = "fulfululkjhjkhfukfukfkffulfululkjhjkhfukfukfkfstjstjtjhgfghfdyjdyjdjdfulfululkjhjkhfukfukfkffulfululkjhjkhfukfukfkf"

training =  [furElise, moonLightSonata,secondWaltz, preludeInCMajor, caspersLullaby]