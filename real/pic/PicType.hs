-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--	PIC: Particle in Cell
--      1990 August
--
module PicType (Count(..), Indx(..), Value(..), Mesh(..), MeshAssoc(..), Electric(..),
		Phi(..), Rho(..), Position(..), Range(..), Velocity(..), ParticleHeap(..)) where

type	Count		= Int
type	Indx		= Int
type	Value		= Double
type	Mesh		= Array (Int,Int) Value
type	MeshAssoc	= Assoc (Int,Int) Value
type	Electric	= (Mesh, Mesh)
type	Phi		= Mesh
type	Rho		= Mesh
type	Position	= (Value, Value)
type	Range		= [Indx]
type	Velocity	= (Value, Value)
type	ParticleHeap	= ([Position], [Velocity])
