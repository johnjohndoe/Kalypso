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
 * Abstracte Klasse AbstractNode von der LeafNode und NoneLeafNode erben.
 * Definiert und implementiert gemeinsame Felder und Operationen
 * Definiert abstrakte Methoden.
 *
 * @version     1.0
 * @author     Wolfgang Bär
 */
public abstract class Node {
    protected transient PageFile file;
    protected HyperBoundingBox unionMinBB;
    protected HyperBoundingBox[] hyperBBs;
    protected int counter;
    protected int pageNumber;
    protected int parentNode;
    protected int place;

    /**
     * Konstruktor der Klasse AbstractNode.
     * @param pageNumber PageNumber des Knotens
     * @param pageFile PageFile zum Knoten gehörende PageFile
     */
    public Node( int pageNumber, PageFile pageFile ) {
        this.file = pageFile;
        this.pageNumber = pageNumber;
        parentNode = 0;
        hyperBBs = new HyperBoundingBox[file.getCapacity()];

        for ( int i = 0; i < file.getCapacity(); i++ )
            hyperBBs[i] = HyperBoundingBox.getNullHyperBoundingBox( file.getDimension() );

        unionMinBB = HyperBoundingBox.getNullHyperBoundingBox( file.getDimension() );
        counter = 0;
    }

    /**
     * Fügt Daten in AbstractNode ein.
     * @param obj einzufügendes Objekt (Typ Integer oder AbstractNode)
     * @param box BoundingBox des Objektes
     */
    public abstract void insertData( Object obj, HyperBoundingBox box );

    /** Löscht Eintrag index aus dem Knoten
     * @param index des Eintrages
     */
    public abstract void deleteData( int index );

    /**
     * Holt Daten aus AbstractNode.
     * @param index des Eintrages
     */
    public abstract Object getData( int index );

    /**
     * Gibt den Vater-Knoten zurück.
     * @return AbstractNode Vater des aktuellen Knotens.
     */
    public Node getParent() {
        Node node = null;

        try {
            node = file.readNode( parentNode );
        } catch ( PageFileException e ) {
            System.out.println( "PageFileException: AbstractNode.getParent() - readNode" );
        }

        return node;
    }

    /** Gibt die PageFile-Nummer zurück, wo Knoten gespeichert ist.
     * @return int Page-Nummer
     */
    public int getPageNumber() {
        return pageNumber;
    }

    /** Setzt die PageFile-Nummer, wo Knoten gespeichert.
     * @param number Page-Nummer
     */
    public void setPageNumber( int number ) {
        this.pageNumber = number;
    }

    /**
     * Derzeit belegter Platz im Knoten.
     * @return int belegter Platz im Knoten
     */
    public int getUsedSpace() {
        return counter;
    }

    /**
     * Gemeinsame HyperBoundingBox über alle Einträge im Knoten.
     * @return HyperBoundingBox UnionMinBB     *
     */
    public HyperBoundingBox getUnionMinBB() {
        return unionMinBB;
    }

    /**
     *
     */
    protected void updateNodeBoundingBox() {
        this.unionMinBB = HyperBoundingBox.getNullHyperBoundingBox( file.getDimension() );

        for ( int i = 0; i < this.getUsedSpace(); i++ )
            this.unionMinBB = this.unionMinBB.unionBoundingBox( this.hyperBBs[i] );
    }

    /**
     * Array von HyperBoundingBoxen der Einträge im Knoten.
     * Array kann leer sein ! Anzahl belegter Plätze siehe getUsedSpace.
     * @return HyperBoundingBox[] Boxes der Einträge
     * @see #getUsedSpace()
     */
    public HyperBoundingBox[] getHyperBoundingBoxes() {
        return hyperBBs;
    }

    /**
     * HyperBoundingBox für Eintrag index im Knoten.
     * @param index des Eintrages
     * @return HyperBoundingBox Box für den Eintrag
     */
    public HyperBoundingBox getHyperBoundingBox( int index ) {
        return hyperBBs[index];
    }

    /**
     * Prüft ob Knoten Rootknoten ist.
     * @return boolean true, wenn root
     */
    public boolean isRoot() {
        return pageNumber == 0;
    }

    /**
     * Tiefe Kopie ohne Dateneinträge (nur HyperBoundingBoxes)
     * Überschreibt Methode clone in Object.
     * @see java.lang.Object#clone()
     */
    public abstract Object clone();

    /**
     * String-Repräsentation des Knotens
     */
    public String toString() {
        String str = "";

        if ( this instanceof LeafNode ) {
            str = "LeafNode: " + unionMinBB.toString();
        } else {
            str = "NoneLeafNode: " + unionMinBB.toString();
        }

        return str;
    }
}