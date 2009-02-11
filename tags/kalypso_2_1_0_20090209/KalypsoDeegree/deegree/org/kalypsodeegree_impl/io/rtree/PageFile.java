/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.io.rtree;

/**
 * Abstrakte Klasse für eine PageFile Definiert Methode, die jede PageFile besitzen muß.
 * 
 * @version 1.0
 * @author Wolfgang Bär
 */
public abstract class PageFile
{
  /** Kapazität eines Knotens */
  protected int m_capacity;

  /** Dimension der Daten */
  protected int m_dimension;

  /** minimale Beladung eines Knotens */
  protected int m_minimum;

  /**
   * Dimension der Daten in der PageFile
   * 
   * @return int - Dimension
   */
  public int getDimension()
  {
    return m_dimension;
  }

  /**
   * Minimale Beladung der Knoten in der PageFile
   * 
   * @return int - minimale Beladung
   */
  public int getMinimum()
  {
    return m_minimum;
  }

  /**
   * Kapazität der Knoten in der PageFile. Kapazität ist der Maximale Dateninhalt plus 1 für OverFlow.
   * 
   * @return int - Kapazität
   */
  public int getCapacity()
  {
    return m_capacity;
  }

  /**
   * Liest einen Knoten aus der PageFile.
   * 
   * @param pageNumber
   *          PageFileNummer, wo Knoten gespeichert ist
   * @return AbstractNode Knoten
   * @throws PageFileException
   */
  public abstract Node readNode( int pageNumber ) throws PageFileException;

  /**
   * Schreibt einen Knoten in PageFile. Methode muß prüfen, ob Knoten eine PageNumber besitzt, ansonsten wird eine neu
   * zugewiesen und zurückgegeben.
   * 
   * @param node
   *          zu schreibender Knoten
   * @return int PageFileNummer, wo Knoten gepeichert.
   * @throws PageFileException
   */
  public abstract int writeNode( Node node ) throws PageFileException;

  /**
   * Markiert einen Knoten in der PageFile als gelöscht.
   * 
   * @param pageNumber
   *          PageFilenummer
   * @return AbstractNode gelöschter Knoten
   */
  public abstract Node deleteNode( int pageNumber ) throws PageFileException;

  /**
   * Initialisiert die PageFile.
   * 
   * @param dimension
   *          der Daten
   * @param capacity
   *          Kapazität der Knoten
   * @throws PageFileException
   */
  public void initialize( int dimension, int capacity ) throws PageFileException 
  {
    this.m_dimension = dimension;
    this.m_capacity = capacity;
    this.m_minimum = (int)Math.round( ( capacity - 1 ) * 0.5 );

    if( this.m_minimum < 2 )
    {
      this.m_minimum = 2;
    }
  }

  /**
   * Closes the pagefile and frees the underlying recourses.
   */
  public abstract void close() throws PageFileException;
}