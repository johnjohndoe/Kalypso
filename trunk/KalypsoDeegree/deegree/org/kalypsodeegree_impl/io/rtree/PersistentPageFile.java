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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import java.util.Stack;


/**
 * Persistente Implementierung einer PageFile.
 * Implementiert als RandomAccesFile.
 *
 * Aufbau der Datei
 *
 * -- Header --
 * int pageFileVersion
 * int dimension
 * int capacity   = maximum + 1 for overflow
 * int minimum
 *
 * -- Body --
 * aufeinanderfolgende Pages mit
 * int typ - 1 LeafNode 2 NoneLeafNode
 * int place - Platz, wo Knoten im Vaterknoten steht
 * int counter - Derzeit benutzer Platz im Knoten
 * int parentNode - Nummer der PageFile des Vaterknotens
 * int pageNumber - eigene PageFile-Nummer
 * - for(i = 0; i < capacity; i++)
 * int data Eintrag i - pageNumber Kindknoten oder Objekt-ID der Dateneinträge
 * - jeweils Abhängigkeit von dimension = x
 *     double pMin x.Dimension - pMin der gemeinsamen HyperBoundingBox
 *     double pMax x.Dimension - pMax der gemeinsamen HyperBoundingBox
 *     - for(i = 0; i < capacity; i++)
 *     double pMin x.Dimension - pMin HyperBB für Eintrag i
 *     double pMax x.Dimension - pMax HyperBB für Eintrag i
 *
 * int entspr. 4 Bytes - double entspr. 8 Bytes
 *
 * PageSize = (4 * (5 + capacity)) + (capacity + 1) * (dimension * 16)
 *
 *
 * @version     1.0
 * @author        Wolfgang Bär
 */
public class PersistentPageFile extends PageFile {
    private static final int PAGEFILE_VERSION = 060676002;
    private static final int EMPTY_PAGE = -22;
    private RandomAccessFile file;
    private Stack emptyPages;
    private String fileName;
    private byte[] buffer;
    private boolean closed;
    private int pageSize;

    /**
     * Konstruktor für PersistentPageFile.
     * @param fileName
     */
    public PersistentPageFile( String fileName ) {
        super();
        this.fileName = fileName;
        this.emptyPages = new Stack();
        this.closed = false;
    }

    /**
     * Initialisiert die PageFile.
     * Überschreibt initialize() in PageFile.
     * @param dimension der Daten
     * @param capacity Kapazität der Knoten
     * @throws PageFileException
     */
    public void initialize( int dimension, int capacity ) throws PageFileException {
        // Initialisierung
        super.initialize( dimension, capacity );

        File fileTest = new File( fileName );

        try {
            if ( dimension == -999 ) {
                // Initialisierung aus bestehender Datei
                if ( !fileTest.exists() ) {
                    throw new PageFileException( "Datei exisitiert nicht !" );
                }

                file = new RandomAccessFile( fileTest, "rw" );

                //	Überprüfung der Korrektheit der Datei
                file.seek( 0 );

                if ( file.readInt() != PAGEFILE_VERSION ) {
                    throw new PageFileException( "Keine PersistenPageFile oder falsche Version !" );
                }

                // Auslesend der Dimension - Initialisierung PageFile
                this.dimension = file.readInt();
                this.capacity = file.readInt();
                this.minimum = file.readInt();
                this.pageSize = ( ( 4 * ( 5 + this.capacity ) ) + 
                                ( ( this.capacity + 1 ) * ( this.dimension * 16 ) ) );
                this.buffer = new byte[pageSize];

                // Einlesen leerer Seiten
                int i = 0;

                try {
                    while ( true ) {
                        file.seek( 16 + ( i * pageSize ) );

                        if ( EMPTY_PAGE == file.readInt() ) {
                            emptyPages.push( new Integer( i ) );
                        }

                        i++;
                    }
                } catch ( IOException ioe ) {
                }
            } else {
                // neue Datei
                file = new RandomAccessFile( fileTest, "rw" );
                file.setLength( 0 );
                this.pageSize = ( ( 4 * ( 5 + capacity ) ) + 
                                ( ( capacity + 1 ) * ( dimension * 16 ) ) );
                this.buffer = new byte[pageSize];

                // header schreiben (Dimension , Dateiversion)
                file.seek( 0 );
                file.writeInt( PAGEFILE_VERSION );
                file.writeInt( this.dimension );
                file.writeInt( this.capacity );
                file.writeInt( this.minimum );
            }
        } catch ( IOException e ) {
            e.fillInStackTrace();
            throw new PageFileException( "IOException occured: \n " + e.getMessage() );
        }
    }

    /**
     * @see PageFile#readNode(int)
     */
    public Node readNode( int pageNumber ) throws PageFileException {
        Node node = null;

        try {
            file.seek( 16 + ( pageNumber * pageSize ) );

            int read = file.read( buffer );

            if ( pageSize == read ) {
                DataInputStream ds = new DataInputStream( new ByteArrayInputStream( buffer ) );

                int type = ds.readInt();

                if ( type == 1 ) {
                    node = new LeafNode( -1, this );
                } else {
                    node = new NoneLeafNode( -1, this );
                }

                node.place = ds.readInt();
                node.counter = ds.readInt();
                node.parentNode = ds.readInt();
                node.pageNumber = ds.readInt();

                if ( type == 1 ) {
                    for ( int i = 0; i < capacity; i++ )
                        ( (LeafNode)node ).data[i] = ds.readInt();
                } else {
                    for ( int i = 0; i < capacity; i++ )
                        ( (NoneLeafNode)node ).childNodes[i] = ds.readInt();
                }

                node.unionMinBB = readNextHyperBoundingBox( ds );

                for ( int i = 0; i < capacity; i++ )
                    node.hyperBBs[i] = readNextHyperBoundingBox( ds );

                ds.close();
            } else {
                throw new PageFileException( "Fehler in bei PageFile-Lesen" );
            }

            return node;
        } catch ( IOException e ) {
            e.fillInStackTrace();
            throw new PageFileException( "PageFileException occured ! \n " + e.getMessage() );
        }
    }

    /**
     *
     *
     * @param ds 
     *
     * @return 
     *
     * @throws IOException 
     */
    public HyperBoundingBox readNextHyperBoundingBox( DataInputStream ds )
                                              throws IOException {
        double[] point1;
        double[] point2;
        point1 = new double[dimension];
        point2 = new double[dimension];

        for ( int i = 0; i < dimension; i++ )
            point1[i] = ds.readDouble();

        for ( int i = 0; i < dimension; i++ )
            point2[i] = ds.readDouble();

        return new HyperBoundingBox( new HyperPoint( point1 ), new HyperPoint( point2 ) );
    }

    /**
     * @see PageFile#writeNode(Node)
     */
    public int writeNode( Node node ) throws PageFileException {
        try {
            if ( node.pageNumber < 0 ) {
                if ( !emptyPages.empty() ) {
                    node.setPageNumber( ( (Integer)emptyPages.pop() ).intValue() );
                } else {
                    node.setPageNumber( (int)( ( file.length() - 16 ) / pageSize ) );
                }
            }

            ByteArrayOutputStream bs = new ByteArrayOutputStream( pageSize );
            DataOutputStream ds = new DataOutputStream( bs );

            int type;

            if ( node instanceof LeafNode ) {
                type = 1;
            } else {
                type = 2;
            }

            ds.writeInt( type );

            ds.writeInt( node.place );
            ds.writeInt( node.counter );
            ds.writeInt( node.parentNode );
            ds.writeInt( node.pageNumber );

            if ( node instanceof LeafNode ) {
                for ( int i = 0; i < node.counter; i++ ) {
                    ds.writeInt( ( (LeafNode)node ).data[i] );
                }

                for ( int i = 0; i < ( capacity - node.counter ); i++ )
                    ds.writeInt( -1 );
            } else {
                for ( int i = 0; i < node.counter; i++ ) {
                    ds.writeInt( ( (NoneLeafNode)node ).childNodes[i] );
                }

                for ( int i = 0; i < ( capacity - node.counter ); i++ )
                    ds.writeInt( -1 );
            }

            for ( int i = 0; i < dimension; i++ )
                ds.writeDouble( node.unionMinBB.getPMin().getCoord( i ) );

            for ( int i = 0; i < dimension; i++ )
                ds.writeDouble( node.unionMinBB.getPMax().getCoord( i ) );

            for ( int j = 0; j < node.counter; j++ ) {
                for ( int i = 0; i < dimension; i++ )
                    ds.writeDouble( node.hyperBBs[j].getPMin().getCoord( i ) );

                for ( int i = 0; i < dimension; i++ )
                    ds.writeDouble( node.hyperBBs[j].getPMax().getCoord( i ) );
            }

            for ( int j = 0; j < ( capacity - node.counter ); j++ ) {
                for ( int i = 0; i < ( dimension * 2 ); i++ )
                    ds.writeDouble( -1 );
            }

            ds.flush();
            bs.flush();

            file.seek( 16 + ( pageSize * node.pageNumber ) );

            file.write( bs.toByteArray() );

            ds.close();

            return node.pageNumber;
        } catch ( IOException e ) {
            e.fillInStackTrace();
            throw new PageFileException( "PageFileException occured ! \n " + e.getMessage() );
        }
    }

    /**
     * @see PageFile#deleteNode(int)
     */
    public Node deleteNode( int pageNumber ) throws PageFileException {
        Node node = this.readNode( pageNumber );

        try {
            file.seek( 16 + ( pageSize * node.pageNumber ) );
            file.writeInt( EMPTY_PAGE );
        } catch ( IOException e ) {
            e.fillInStackTrace();
            throw new PageFileException( "PageFileException occured ! \n " + e.getMessage() );
        }

        emptyPages.push( new Integer( pageNumber ) );
        return node;
    }

    /**
     * @see PageFile#close()
     */
    public void close() throws PageFileException {
        try {
            file.close();
        } catch ( IOException e ) {
            e.fillInStackTrace();
            throw new PageFileException( "PageFileException during close()" );
        }

        closed = true;
    }

    /**
     *
     *
     * @throws Throwable 
     */
    public void finalize() throws Throwable {
        if ( !closed ) {
            file.close();
        }

        super.finalize();
    }
}