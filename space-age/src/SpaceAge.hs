module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / yearLengthOn planet

yearLengthOn :: Planet -> Float
yearLengthOn Earth = 31557600.0
yearLengthOn Mercury = yearLengthOn Earth * 0.2408467
yearLengthOn Venus = yearLengthOn Earth * 0.61519726
yearLengthOn Mars = yearLengthOn Earth * 1.8808158
yearLengthOn Jupiter = yearLengthOn Earth * 11.862615
yearLengthOn Saturn = yearLengthOn Earth * 29.447498
yearLengthOn Uranus = yearLengthOn Earth * 84.016846
yearLengthOn Neptune = yearLengthOn Earth * 164.79132
