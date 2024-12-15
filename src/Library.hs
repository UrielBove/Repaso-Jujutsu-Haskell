module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Nombre = String
type Antiguedad = Number
type Grado = Number
type Clan = String
type Grupo = [Hechicero]

data Hechicero = UnHechicero {
    nombre :: String,
    antiguedad :: Antiguedad, 
    grado :: Grado,
    clan :: Clan
} deriving (Show)

nobara :: Hechicero
nobara = UnHechicero "Nobara" 1 3 "Kugisaki" 

satoru :: Hechicero
satoru = UnHechicero "Satoru" 15 0 "Gojo"

maki :: Hechicero
maki = UnHechicero "Maki" 3 4 "Zenin" 

yuji :: Hechicero 
yuji = UnHechicero "Yuji" 0 1 "Itadori" 

clanesPrestigiosos = ["Zenin", "Kamo", "Gojo"]

grupoA :: [Hechicero]
grupoA = [nobara, yuji, satoru, maki]

estaPreparado :: [Hechicero] -> Bool
estaPreparado = (>3).length 

esInvencible :: [Hechicero] -> Bool
esInvencible = any esEspecial 

esEspecial :: Hechicero -> Bool
esEspecial = (==0).grado

--func :: a -> (b -> (c -> (d -> (e))))

esPrestigioso :: Hechicero -> Bool
esPrestigioso hechicero =  clan hechicero `elem` clanesPrestigiosos

esPrestigioso2 :: Hechicero -> Bool
esPrestigioso2 = (`elem` clanesPrestigiosos). clan

esFavorito :: Grupo -> Bool
esFavorito  = all esPrestigioso 

expertos :: Grupo -> Grupo
expertos = filter ((>1).antiguedad) 

subirGrado :: Hechicero -> Hechicero
subirGrado hechicero | grado hechicero > 0 = hechicero{grado = grado hechicero - 1}
    |otherwise = hechicero

--8 

puedeHacerFrenteAMaldicion :: Grupo -> Bool
puedeHacerFrenteAMaldicion grupo = esInvencible grupo|| estaPreparado grupo

powerUp :: Grupo -> Grupo
powerUp = map subirGrado

--9

type Criterio a = Hechicero -> a

elegirHechiceroMision :: Ord a => Hechicero -> Hechicero -> Criterio a -> Hechicero
elegirHechiceroMision hechicero1 hechicero2 criterio
    |criterio hechicero1 >= criterio hechicero2 = hechicero1
    |otherwise = hechicero2

nivelTryhard :: Hechicero -> Number
nivelTryhard hechicero = 1 / (grado hechicero + 1)

nivelBurocratico :: Hechicero -> Number
nivelBurocratico = length . clan

nivelIntimidante :: Hechicero -> Char
nivelIntimidante = maximum . clan

nivelDeSigilo :: Hechicero -> Number
nivelDeSigilo = (*6) . antiguedad

