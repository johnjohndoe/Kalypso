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
 * Implementierung eines Zwischen-Knotens.
 * Erbt Methoden von AbstractNode.
 * Implementiert abstrakte Methoden.
 * @version     1.0
 * @author        Wolfgang Bär
 */
public class NoneLeafNode extends Node {
    protected int[] childNodes;

    /**
     * Konstruktor NoneLeaf-AbstractNode.
     * @param pageNumber
     * @param file PageFile des Knotens
     */
    public NoneLeafNode( int pageNumber, PageFile file ) {
        super( pageNumber, file );
        childNodes = new int[file.getCapacity()];

        for ( int i = 0; i < file.getCapacity(); i++ )
            childNodes[i] = -1;
    }

    /**
     * Holt Kindknoten an Stelle index.
     * Object ist vom Typ AbstractNode.
     * @param index des Kindknoten
     * @return Object KindKnoten
     */
    public Object getData( int index ) {
        Object obj = null;

        try {
            obj = file.readNode( childNodes[index] );
        } catch ( PageFileException e ) {
            System.out.println( "PageFileException NoneLeafNode.getData \n" + e.getMessage() );
        }

        return obj;
    }

    /**
     * Fügt Kindknoten als Eintrag zum Knoten hinzu.
     * Überprüft keine Aufnahmefähigkeit des Knotens
     * @param node einzufügender Kindknoten (Type AbstractNode)
     * @param box des Kindknoten
     */
    public void insertData( Object node, HyperBoundingBox box ) {
        childNodes[counter] = ( (Node)node ).getPageNumber();
        hyperBBs[counter] = box;
        unionMinBB = unionMinBB.unionBoundingBox( box );
        ( (Node)node ).parentNode = this.pageNumber;
        ( (Node)node ).place = this.counter;
        counter++;

        try {
            file.writeNode( (Node)node );
        } catch ( PageFileException e ) {
            System.out.println( 
                    "PageFileException NoneLeafNode.insertData - bei writeNode(AbstractNode) \n" + 
                    e.getMessage() );
        }
    }

    /**
     * Löscht Kindknoten Eintrag an Stelle index.
     * @param index des Eintrages.
     */
    public void deleteData( int index ) {
        if ( this.getUsedSpace() == 1 ) {
            // only one element is a special case.
            hyperBBs[0] = HyperBoundingBox.getNullHyperBoundingBox( file.getDimension() );
            childNodes[0] = -1;
            counter--;
        } else {
            System.arraycopy( hyperBBs, index + 1, hyperBBs, index, counter - index - 1 );
            System.arraycopy( childNodes, index + 1, childNodes, index, counter - index - 1 );
            hyperBBs[counter - 1] = HyperBoundingBox.getNullHyperBoundingBox( file.getDimension() );
            childNodes[counter - 1] = -1;
            counter--;

            for ( int i = 0; i < counter; i++ ) {
                Node help = (Node)this.getData( i );
                help.place = i;

                try {
                    file.writeNode( help );
                } catch ( PageFileException e ) {
                    System.out.println( 
                            "PageFileException NoneLeafNode.deleteData - bei writeNode(AbstractNode) \n" + 
                            e.getMessage() );
                }
            }
        }

        updateNodeBoundingBox();
    }

    /**
     * Gibt den Index des Eintrages mit geringster Vergrößerung zurück.
     * Gibt den Index des Eintrages zurück, dessen BoundingBox am geringsten
     * bei Hinzunahme der übergebenen HyperBoundingBox vergrößert wird.
     * @param box für die der Index bestimmt werden soll.
     * @return int Index des Eintrages
     */
    public int getLeastEnlargement( HyperBoundingBox box ) {
        double[] area = new double[counter];

        for ( int i = 0; i < counter; i++ )
            area[i] = ( hyperBBs[i].unionBoundingBox( box ) ).getArea() - hyperBBs[i].getArea();

        double min = area[0];
        int minnr = 0;

        for ( int i = 1; i < counter; i++ ) {
            if ( area[i] < min ) {
                min = area[i];
                minnr = i;
            }
        }

        return minnr;
    }

    /**
     * Erstellt eine Kopie des NoneLeafNodes.
     * Tiefe Kopie bis auf Referenz auf PageFile.
     * @return Object NoneLeafNode-Kopie
     */
    public Object clone() {
        NoneLeafNode clone = new NoneLeafNode( this.pageNumber, this.file );
        clone.counter = this.counter;
        clone.place = this.place;
        clone.unionMinBB = (HyperBoundingBox)this.unionMinBB.clone();
        clone.parentNode = this.parentNode;

        for ( int i = 0; i < file.getCapacity(); i++ )
            clone.hyperBBs[i] = (HyperBoundingBox)this.hyperBBs[i].clone();

        return clone;
    }
}