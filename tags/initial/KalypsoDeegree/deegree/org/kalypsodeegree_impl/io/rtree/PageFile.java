/*----------------    FILE HEADER  ------------------------------------------

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Contact:

Copyright (C) 2002 Wolfgang Baer - WBaer@gmx.de
 
Adapted May 2003 by IDgis, The Netherlands - www.idgis.nl
                 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.io.rtree;

/**
 * Abstrakte Klasse für eine PageFile
 * Definiert Methode, die jede PageFile besitzen muß.
 *
 * @version     1.0
 * @author        Wolfgang Bär
 */
public abstract class PageFile {
    /** Kapazität eines Knotens */
    protected int capacity;

    /** Dimension der Daten */
    protected int dimension;

    /** minimale Beladung eines Knotens */
    protected int minimum;

    /**
     * Dimension der Daten in der PageFile
     * @return int - Dimension
     */
    public int getDimension() {
        return dimension;
    }

    /**
     * Minimale Beladung der Knoten in der PageFile
     * @return int - minimale Beladung
     */
    public int getMinimum() {
        return minimum;
    }

    /**
     * Kapazität der Knoten in der PageFile.
     * Kapazität ist der Maximale Dateninhalt plus 1 für OverFlow.
     * @return int - Kapazität
     */
    public int getCapacity() {
        return capacity;
    }

    /**
     * Liest einen Knoten aus der PageFile.
     * @param pageNumber PageFileNummer, wo Knoten gespeichert ist
     * @return AbstractNode Knoten
     * @throws PageFileException
     */
    public abstract Node readNode( int pageNumber ) throws PageFileException;

    /**
     * Schreibt einen Knoten in PageFile.
     * Methode muß prüfen, ob Knoten eine PageNumber besitzt,
     * ansonsten wird eine neu zugewiesen und zurückgegeben.
     * @param node zu schreibender Knoten
     * @return int PageFileNummer, wo Knoten gepeichert.
     * @throws PageFileException
     */
    public abstract int writeNode( Node node ) throws PageFileException;

    /**
     * Markiert einen Knoten in der PageFile als gelöscht.
     * @param pageNumber PageFilenummer
     * @return AbstractNode gelöschter Knoten
     */
    public abstract Node deleteNode( int pageNumber ) throws PageFileException;

    /**
     * Initialisiert die PageFile.
     * @param dimension der Daten
     * @param capacity Kapazität der Knoten
     * @throws PageFileException
     */
    public void initialize( int dimension, int capacity ) throws PageFileException {
        this.dimension = dimension;
        this.capacity = capacity;
        this.minimum = (int)Math.round( ( capacity - 1 ) * 0.5 );

        if ( this.minimum < 2 ) {
            this.minimum = 2;
        }
    }

    /**
     * Closes the pagefile and frees the underlying recourses.
     */
    public abstract void close() throws PageFileException;
}