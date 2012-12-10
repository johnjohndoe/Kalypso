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
package org.kalypso.model.hydrology.binding.initialValues;

import java.util.Date;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * * Binding class for {http://www.tuhh.de/initialValues}InitialValues
 * 
 * @author Gernot Belger
 */
public class InitialValues extends Feature_Impl
{
  public static final String NS_INIVALUES = NaModelConstants.NS_INIVALUES;

  private static final QName PROP_INI_DATE = new QName( NS_INIVALUES, "iniDate" ); //$NON-NLS-1$

  private static final QName MEMBER_CATCHMENT = new QName( NS_INIVALUES, "catchmentMember" ); //$NON-NLS-1$

  private static final QName MEMBER_CHANNEL = new QName( NS_INIVALUES, "channelMember" ); //$NON-NLS-1$

  private FeatureBindingCollection<Channel> m_channels;

  private FeatureBindingCollection<Catchment> m_catchments;

  public InitialValues( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public Date getInitialDate( )
  {
    return DateUtilities.toDate( getProperty( PROP_INI_DATE ) ); 
  }

  public void setInitialDate( final Date newInitialDate )
  {
    setProperty( PROP_INI_DATE, DateUtilities.toXMLGregorianCalendar( newInitialDate ) );
  }

  public synchronized IFeatureBindingCollection<Catchment> getCatchments( )
  {
    if( m_catchments == null )
      m_catchments = new FeatureBindingCollection<Catchment>( this, Catchment.class, MEMBER_CATCHMENT );

    return m_catchments;
  }

  public synchronized IFeatureBindingCollection<Channel> getChannels( )
  {
    if( m_channels == null )
      m_channels = new FeatureBindingCollection<Channel>( this, Channel.class, MEMBER_CHANNEL );

    return m_channels;
  }


}
