module DataTypes where

import IntCoor

data Dinamica = Dinamica {
          getOutputNam   :: String,
          getAtomN       :: Int,
          getRootN       :: Int,
          getStartRlxRt  :: Int,
          getDT          :: Double,
          getAtomT       :: [String],
          getEnergies    :: [[Double]],
          getCoordinates :: [Vec Double],
          getOscStr      :: [Double],
          getCharTran    :: [Double]
          } deriving Show

data PlotType = Pop | Ene | Dyn deriving (Eq,Show)

--data PlotType2 = Red | Black | Hop deriving (Eq,Show)

data Plottable = Cccc 
               | CcccCorrected 
               | Beta 
               | BetaCorrected 
               | Tau 
               | Delta 
               | Bla 
               | Ct 
               | Root 
               | Jump 
               | EnergyDyn   
               | EnergyDiff  Root Root
               | Energy      Root
               | Population  Root
               | Internal [Int]
               | Charge [Int]
               deriving (Eq, Show, Read)

data Root = S0 | S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Show, Read, Enum)

data Flag = Help
            | CreateInfo     String
            | CreateInfoBin  String
            | CreateInfoQMMM String
            | CheckInfo      String
            | InputFile      String
            | DoAll          String
            | Doall          String
            | Quick          String
            | Dani           String
            deriving (Show, Eq)

data IsomType = Cis | Trans deriving (Show, Read)

-- This is declared into ParseInput module
--
--data Inputs = Inputs {
--     getfolder            :: String,        -- Here Info foldername
--     getchargeTrFragment  :: [Int],         -- Here list of Atom in charge transfer fraction
--     getchargeTrThresh    :: [Double],      -- Here list of Threshold for CT graphics
--     getccccList          :: [Int],         -- Here the central dihedral
--     getbetaList          :: [Int],         -- Here beta angle
--     getblaList           :: [[(Int,Int)]], -- BLA list of single bonds, list of double bonds
--     getisomType          :: IsomType,      -- This is your cutting line if you wanna shift down graphics
--     getnRoot             :: Int,           -- This is the root number
--     getListToPlot        :: [Plottable]    -- These are the DATA file column
--     getGnuplotOptions    :: String         -- Those are gnuplot options
--     } deriving Show

type AllTrajData = [SingleTrajData]
type SingleTrajData = [PlottableData]
type PlottableData = [String]

data FileType = Binary | Normal deriving Show

convFStoAU = 41.3414472
convAUtoFS =  0.0241888

