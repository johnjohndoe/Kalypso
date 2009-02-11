/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.observation.result;

import java.util.Comparator;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.observation.phenomenon.IPhenomenon;

/**
 * Each component is a comparator of its own values. That is, if the tuple-result gets sorted by one columne (i.e.
 * component), the values of the records are sorted by this comparator.
 * 
 * @author Marc Schlienger
 */
public interface IComponent extends Comparator<Object>
{
  /**
   * Id or internal name of this component. For example if this component really was read from a dictionary, the id
   * should be the urn of the corresponding dictionary entry.
   */
  public String getId( );

  /**
   * User-fired name of this component.
   */
  public String getName( );

  public String getDescription( );

  public String getUnit( );

  public String getFrame( );

  public QName getValueTypeName( );

  public Object getDefaultValue( );

  public IRestriction[] getRestrictions( );

  public IPhenomenon getPhenomenon( );
  
  /** 
   * @return a precision to compare numeric values
   */
  public Double getPrecision();

  /** override equals. Component are equals if their name, description, valueTyleName and defaultValue are equals */
  public boolean equals( final Object object );

  /** override hashCode according to equals */
  public int hashCode( );
}