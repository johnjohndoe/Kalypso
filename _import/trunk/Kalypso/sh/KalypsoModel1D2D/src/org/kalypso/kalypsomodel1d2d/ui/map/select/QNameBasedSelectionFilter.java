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
package org.kalypso.kalypsomodel1d2d.ui.map.select;

import java.util.HashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A Selection filter based on a list of QName.
 * The behavior of accept can be controled be seting the 
 * {@link #acceptSubstituables}  member:
 * <ul>
 *      <li/> if true features will be accepted when the 
 *          substituable to feature with qname in the {@link #selectableFeatureQName} set
 *      <li/>if false a feature are selected if and only its QName is
 *       contained in the set of selectable features  
 * </ul>
 * 
 * 
 * @author Patrice Congo
 *
 */
public class QNameBasedSelectionFilter implements ISelectionFilter
{
  /** 
   * Conatins the QName the feature to test should have or should be
   * substituable to the specified type
   */
  private Set<QName> selectableFeatureQName= new HashSet<QName>();
  
  /**
   * flag that specifies whether the test should test for substitution 
   * if true or exact QName equality otherwise 
   * 
   */
  private boolean acceptSubstituables=false;
  
  
  /**
   * Creates a new {@link QNameBasedSelectionFilter} that use
   * exact Qname matching
   * 
   */
  public QNameBasedSelectionFilter()
  {
    this(false);
  }
  
  /**
   * Creates a new {@link QNameBasedSelectionFilter}.
   * The acceptance behavior is is specified by
   * the parameter acceptSubstituables
   * @param acceptSubstiuables customized the acceptane behavior
   *    <ul>
   *        <li/> if true {@link #accept(Feature)} use substitution
   *            hierarchy check
   *        <li/> if false {@link #accept(Feature)} use exact 
   *        QName matching
   *    </ul>  
   * 
   */
  public QNameBasedSelectionFilter(boolean acceptSubstiuables)
  {
    this.acceptSubstituables=acceptSubstiuables;
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.ISelectionFilter#accept(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean accept( Feature feature)
  {
    if(feature==null)
    {
      return false;
    }
    IFeatureType featureType = feature.getFeatureType();
    QName featureQName = featureType.getQName();    
    if(acceptSubstituables)
    {
      for(QName curName:selectableFeatureQName)
      {
        if(GMLSchemaUtilities.substitutes( featureType, curName ))
        {
          return true;
        }
      }
      return false;
    }
    else
    {
      return selectableFeatureQName.contains( featureQName );
    }
  }

  /**
   * Add a Qname the the set of accepted feature type
   */
  public boolean add( QName qName )
  {
    return selectableFeatureQName.add( qName );
  }
 
  /**
   * Clear the set of accepted feature types
   */
  public void clear( )
  {
    selectableFeatureQName.clear();
  }

  /**
   * Answer whether the set of selectable is empty or not
   * @return true the the set of accepted feature emty otherwise
   * false 
   */
  public boolean isEmpty( )
  {
    return selectableFeatureQName.isEmpty();
  }
  
  /**
   * Get a copy of acceptable feature {@link QName} set
   */
  public Set<QName> getSelectableFeatureQName( )
  {
    return new HashSet<QName>(selectableFeatureQName);
  }

  /**
   * Remove the specified {@link QName} from the set of 
   * acceptable feature set
   */
  public boolean remove( QName qName )
  {
    return selectableFeatureQName.remove( qName );
  }

  /**
   * Answer whether {@link #accept(Feature)} does use
   * substitution for acceptance check or not
   * @return true if substitution is used for acceptance check
   *    otherwise false
   */
  public boolean doesAcceptSubstituables( )
  {
    return acceptSubstituables;
  }

  /**
   * Customize the acceptance check of {@link #accept(Feature)}:
   * <ul> 
   *    <li/>pass true to set a check based on use substitution
   *    <li/>pass fals to set a check based on exact QName checking
   * </ul>
   * @return true if substitution is used for acceptance check
   *    otherwise false
   */
  public void setAcceptSubstituables( boolean acceptSubstituables )
  {
    this.acceptSubstituables = acceptSubstituables;
  }
  
  public static final QNameBasedSelectionFilter getFilterForQName(QName qNameToFilter)
  {
    QNameBasedSelectionFilter selectionFilter = new QNameBasedSelectionFilter();
    selectionFilter.add( qNameToFilter );    
    return selectionFilter;
  }
  
}
