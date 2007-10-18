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
package org.kalypso.util.swt;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.kalypso.contribs.eclipse.jface.viewers.IFCVDelegate;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.restriction.EnumerationRestriction;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public class FCVFeatureDelegate implements IFCVDelegate
{
  protected final Feature m_feature;

  private final QName m_featureType;

  private final Map<Object, String> m_mapping = new HashMap<Object, String>();

  public FCVFeatureDelegate( final Feature feature, final QName qFeatureType )
  {
    m_feature = feature;
    m_featureType = qFeatureType;
  }

  /**
   * @see org.kalypso.nofdpidss.ui.application.widgets.IWComboViewerDelegate#getDefaultKey()
   */
  public ISelection getDefaultKey( )
  {
    final Object object = m_feature.getProperty( m_featureType );
    if( object != null && object instanceof String )
      return new StructuredSelection( object );

    final Object[] array = m_mapping.keySet().toArray();
    if( array.length > 0 )
      return new StructuredSelection( array[0] );

    return new StructuredSelection();
  }

  /**
   * @see org.kalypso.nofdpidss.ui.application.widgets.IWComboViewerDelegate#getInputData()
   */
  public Object[] getInputData( )
  {
    final IPropertyType property = m_feature.getFeatureType().getProperty( m_featureType );
    if( !(property instanceof IValuePropertyType) )
      throw new IllegalStateException();

    final IValuePropertyType vpt = (IValuePropertyType) property;
    final IRestriction[] restrictions = vpt.getRestriction();
    for( final IRestriction restriction : restrictions )
    {
      if( !(restriction instanceof EnumerationRestriction) )
        continue;

      final EnumerationRestriction r = (EnumerationRestriction) restriction;

      final Object[] enumeration = r.getEnumeration();
      final String[] labels = r.getLabels( "en_EN" );

      if( enumeration.length != labels.length )
        throw new NotImplementedException();

      for( int i = 0; i < enumeration.length; i++ )
        m_mapping.put( enumeration[i], labels[i] );
    }

    return m_mapping.keySet().toArray();
  }

  /**
   * @see org.kalypso.nofdpidss.ui.application.widgets.IWComboViewerDelegate#getValue(java.lang.Object)
   */
  public String getValue( final Object element )
  {
    return m_mapping.get( element );
  }

}
