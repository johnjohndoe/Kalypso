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
 * Implementierung eines Blatt-Knotens.
 * Erbt Methoden von AbstractNode.
 * Implementiert abstrakte Methoden.
 *
 * @version     1.0
 * @author        Wolfgang Bär
 */
public class LeafNode extends Node {
    protected int[] data;

    /**
     * Konstruktor Leaf-AbstractNode.
     * @param pageNumber
     * @param file PageFile des Knotens
     */
    public LeafNode( int pageNumber, PageFile file ) {
        super( pageNumber, file );
        data = new int[file.getCapacity()];

        for ( int i = 0; i < file.getCapacity(); i++ )
            data[i] = -1;
    }

    /**
     * Konstruktor Leaf-AbstractNode.
     * PageNumber wird beim ersten Abspeichern zugewiesen.
     * @param file PageFile des Knotens
     */
    public LeafNode( PageFile file ) {
        super( -1, file );
        data = new int[file.getCapacity()];

        for ( int i = 0; i < file.getCapacity(); i++ )
            data[i] = -1;
    }

    /**
     * Gibt Dateneintrag zurück.
     * Rückgabe ist vom Typ Object (Integer-Objekt).
     * @param index für Eintrag
     * @return Object Dateneintrag
     */
    public Object getData( int index ) {
        return new Integer( data[index] );
    }

    /**
     * Fügt einen Dateneintrag (ObjectID) mit angegebener HyperBoundingBox ein.
     * @param obj Object-ID (muß eine in Integer-Objekt gekapselte int-Zahl sein !!)
     * @param box des Datums
     */
    public void insertData( Object obj, HyperBoundingBox box ) {
        data[counter] = ( (Integer)obj ).intValue();
        hyperBBs[counter] = box;
        unionMinBB = unionMinBB.unionBoundingBox( box );
        counter = counter + 1;
    }

    /** Löscht Daten-Eintrag an Stelle index
     * @param index des Eintrages
     */
    public void deleteData( int index ) {
        if ( this.getUsedSpace() == 1 ) {
            // only one element is a special case.
            hyperBBs[0] = HyperBoundingBox.getNullHyperBoundingBox( file.getDimension() );
            data[0] = -1;
        } else {
            System.arraycopy( hyperBBs, index + 1, hyperBBs, index, counter - index - 1 );
            System.arraycopy( data, index + 1, data, index, counter - index - 1 );
            hyperBBs[counter - 1] = HyperBoundingBox.getNullHyperBoundingBox( file.getDimension() );
            data[counter - 1] = -1;
        }

        counter--;
        updateNodeBoundingBox();
    }

    /**
     * Erstellt eine Kopie des LeafNodes.
     * Tiefe Kopie bis auf Referenz auf PageFile.
     * @return Object LeafNode-Kopie
     */
    public Object clone() {
        LeafNode clone = new LeafNode( this.pageNumber, this.file );
        clone.counter = this.counter;
        clone.place = this.place;
        clone.unionMinBB = (HyperBoundingBox)this.unionMinBB.clone();
        clone.parentNode = this.parentNode;

        for ( int i = 0; i < file.getCapacity(); i++ )
            clone.hyperBBs[i] = (HyperBoundingBox)this.hyperBBs[i].clone();

        return clone;
    }
}