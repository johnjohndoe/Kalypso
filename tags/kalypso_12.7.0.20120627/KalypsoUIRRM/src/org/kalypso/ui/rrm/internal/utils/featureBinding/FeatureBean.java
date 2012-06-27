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
package org.kalypso.ui.rrm.internal.utils.featureBinding;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ObjectUtils;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

/**
 * REMARK: extends {@link AbstractModelObject}, so implementors can add additional properties to a bean.
 * 
 * @author Gernot Belger
 */
public class FeatureBean<F extends Feature>
{
  private final PropertyChangeSupport m_pcs = new PropertyChangeSupport( this );

  private final Map<QName, Object> m_properties = new HashMap<>();

  private final Set<QName> m_dirtyProperties = new HashSet<>();

  private final Set<FeatureBeanObservableValue> m_observables = new HashSet<>();

  private F m_feature;

  private IFeatureType m_featureType;

  public FeatureBean( final QName featureType )
  {
    m_featureType = GMLSchemaUtilities.getFeatureTypeQuiet( featureType );
  }

  public FeatureBean( final F feature )
  {
    setFeature( feature );
  }

  public void addPropertyChangeListener( final PropertyChangeListener listener )
  {
    m_pcs.addPropertyChangeListener( listener );
  }

  public void addPropertyChangeListener( final String propertyName, final PropertyChangeListener listener )
  {
    m_pcs.addPropertyChangeListener( propertyName, listener );
  }

  public void removePropertyChangeListener( final PropertyChangeListener listener )
  {
    m_pcs.removePropertyChangeListener( listener );
  }

  public void removePropertyChangeListener( final String propertyName, final PropertyChangeListener listener )
  {
    m_pcs.removePropertyChangeListener( propertyName, listener );
  }

  public void setFeature( final F feature )
  {
    m_feature = feature;

    m_dirtyProperties.clear();
    m_properties.clear();

    if( m_feature == null )
      return;

    m_featureType = m_feature.getFeatureType();

    final IPropertyType[] properties = m_feature.getFeatureType().getProperties();
    for( final IPropertyType pt : properties )
    {
      if( pt instanceof IValuePropertyType )
      {
        setProperty( pt.getQName(), m_feature.getProperty( pt ) );
      }
    }
  }

  public IFeatureType getFeatureType( )
  {
    return m_featureType;
  }

  public F getFeature( )
  {
    return m_feature;
  }

  public Object getProperty( final QName property )
  {
    return m_properties.get( property );
  }

  public void setProperty( final QName property, final Object value )
  {
    final Object oldValue = getProperty( property );

    m_properties.put( property, value );

    if( m_feature != null )
    {
      final Object oldFeatureValue = m_feature.getProperty( property );
      if( !ObjectUtils.equals( value, oldFeatureValue ) )
        m_dirtyProperties.add( property );
    }

    doFirePropertyChanged( property.toString(), oldValue, value );
  }

  private void doFirePropertyChanged( final String propertyName, final Object oldValue, final Object newValue )
  {
    // FIXME: instead, add observables as listeners to AbstractModelObject and remove this method
    for( final FeatureBeanObservableValue observable : m_observables )
    {
      observable.firePropertyChanged( oldValue, newValue );
    }

    m_pcs.firePropertyChange( propertyName, oldValue, newValue );
  }

  void addObservable( final FeatureBeanObservableValue observable )
  {
    m_observables.add( observable );
  }

  void removeObservable( final FeatureBeanObservableValue observable )
  {
    m_observables.remove( observable );
  }

  public ICommand applyChanges( )
  {
    final CompositeCommand composite = new CompositeCommand( Messages.getString( "FeatureBean_0" ) ); //$NON-NLS-1$

    for( final QName property : m_dirtyProperties )
    {
      final IPropertyType propertyType = m_feature.getFeatureType().getProperty( property );
      final Object value = getProperty( property );

      final ChangeFeatureCommand changeCommand = new ChangeFeatureCommand( m_feature, propertyType, value );
      composite.addCommand( changeCommand );
    }

    m_dirtyProperties.clear();

    return composite;
  }

  /**
   * Returns the currently stored properties in this bean.<br/>
   * The returned {@link Map} is not modifiable.
   */
  public Map<QName, Object> getProperties( )
  {
    return Collections.unmodifiableMap( m_properties );
  }

  public void revert( )
  {
    for( final QName property : m_properties.keySet() )
    {
      final Object value = m_feature.getProperty( property );
      setProperty( property, value );
    }
  }
}