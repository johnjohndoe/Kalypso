/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.wizards.lengthsection;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandlerParameters;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.KalypsoFeatureThemeHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.shape.ShapeCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 *
 */
class CreateLengthSectionData extends AbstractModelObject
{
  /* pseudo type handler as placeholder for null */
  static final IValuePropertyType NO_RPOPERTY_SELECTED = GMLSchemaFactory.createValuePropertyType( new QName( StringUtils.EMPTY, StringUtils.EMPTY ), MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) ), 0, 0, false ); //$NON-NLS-1$

  static final String PROPERTY_STATUS = "status"; //$NON-NLS-1$

  static final String PROPERTY_RIVER_LINE = "riverLineTheme"; //$NON-NLS-1$

  static final String PROPERTY_RIVER_NAME_INPUT = "riverNameInput"; //$NON-NLS-1$

  static final String PROPERTY_RIVER_NAME_PROPERTY = "riverNameProperty"; //$NON-NLS-1$

  static final String PROPERTY_RIVER_NAMES = "riverNames"; //$NON-NLS-1$

  static final String PROPERTY_SELECTED_RIVER_NAME = "selectedRiverName"; //$NON-NLS-1$

  static final String PROPERTY_STATION_FROM_INPUT = "stationFromInput"; //$NON-NLS-1$

  static final String PROPERTY_STATION_FROM_PROPERTY = "stationFromProperty"; //$NON-NLS-1$

  static final String PROPERTY_STATION_TO_INPUT = "stationToInput"; //$NON-NLS-1$

  static final String PROPERTY_STATION_TO_PROPERTY = "stationToProperty"; //$NON-NLS-1$

  static final String PROPERTY_USE_KM_VALUES = "useKmValue"; //$NON-NLS-1$

  static final String PROPERTY_SAMPLING_DISTANCE = "samplingDistance"; //$NON-NLS-1$

  private static final String RIVER_NAME_FIELD = "NAME"; //$NON-NLS-1$

  private static final String FROM_STATION_FIELD = "RIVER_A"; //$NON-NLS-1$

  private static final String TO_STATION_FIELD = "RIVER_B"; //$NON-NLS-1$

  private final IKalypsoFeatureTheme[] m_lineThemes;

  private IStatus m_status = Status.OK_STATUS;

  private IKalypsoFeatureTheme m_riverLineTheme;

  private IValuePropertyType[] m_riverNameInput = new IValuePropertyType[0];

  private IValuePropertyType m_riverNameProperty = null;

  private IValuePropertyType[] m_stationFromInput = new IValuePropertyType[0];

  private IValuePropertyType m_stationFromProperty = null;

  private IValuePropertyType[] m_stationToInput = new IValuePropertyType[0];

  private IValuePropertyType m_stationToProperty = null;

  private String[] m_riverNames = new String[0];

  /** Selected element of the current river list */
  private String m_selectedRiverName = null;

  private boolean m_useKmValue = false;

  private int m_samplingDistance = 100;

  public CreateLengthSectionData( final IMapPanel mapPanel )
  {
    m_lineThemes = KalypsoFeatureThemeHelper.getLineThemes( mapPanel );

    if( m_lineThemes.length > 0 )
      setRiverLineTheme( m_lineThemes[0] );
    else
    {
      final String msg = Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.13" ); //$NON-NLS-1$
      setStatus( new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, msg ) );
    }
  }

  public IKalypsoFeatureTheme getRiverLineTheme( )
  {
    return m_riverLineTheme;
  }

  public void setRiverLineTheme( final IKalypsoFeatureTheme riverLineTheme )
  {
    final IKalypsoFeatureTheme oldValue = m_riverLineTheme;

    m_riverLineTheme = riverLineTheme;

    firePropertyChange( PROPERTY_RIVER_LINE, oldValue, riverLineTheme );

    updateProperties();
  }

  public IStatus getStatus( )
  {
    return m_status;
  }

  public void setStatus( final IStatus status )
  {
    final Object oldValue = m_status;

    m_status = status;

    firePropertyChange( PROPERTY_STATUS, oldValue, status );
  }

  public IValuePropertyType[] getRiverNameInput( )
  {
    return m_riverNameInput;
  }

  public void setRiverNameInput( final IValuePropertyType[] riverNameInput )
  {
    final IValuePropertyType[] oldValue = m_riverNameInput;

    m_riverNameInput = riverNameInput;

    firePropertyChange( PROPERTY_RIVER_NAME_INPUT, oldValue, riverNameInput );
  }

  public IValuePropertyType getRiverNameProperty( )
  {
    return m_riverNameProperty;
  }

  public void setRiverNameProperty( final IValuePropertyType riverNameProperty )
  {
    final IValuePropertyType oldValue = m_riverNameProperty;

    m_riverNameProperty = riverNameProperty;

    firePropertyChange( PROPERTY_RIVER_NAME_PROPERTY, oldValue, riverNameProperty );
  }

  public IValuePropertyType[] getStationFromInput( )
  {
    return m_stationFromInput;
  }

  public void setStationFromInput( final IValuePropertyType[] stationFromInput )
  {
    final IValuePropertyType[] oldValue = m_stationFromInput;

    m_stationFromInput = stationFromInput;

    firePropertyChange( PROPERTY_STATION_FROM_INPUT, oldValue, stationFromInput );
  }

  public IValuePropertyType getStationFromProperty( )
  {
    return m_stationFromProperty;
  }

  public void setStationFromProperty( final IValuePropertyType stationFromProperty )
  {
    final IValuePropertyType oldValue = m_stationFromProperty;

    m_stationFromProperty = stationFromProperty;

    firePropertyChange( PROPERTY_STATION_FROM_PROPERTY, oldValue, stationFromProperty );
  }

  public IValuePropertyType[] getStationToInput( )
  {
    return m_stationToInput;
  }

  public void setStationToInput( final IValuePropertyType[] stationToInput )
  {
    final IValuePropertyType[] oldValue = m_stationToInput;

    m_stationToInput = stationToInput;

    firePropertyChange( PROPERTY_STATION_TO_INPUT, oldValue, stationToInput );
  }

  public IValuePropertyType getStationToProperty( )
  {
    return m_stationToProperty;
  }

  public void setStationToProperty( final IValuePropertyType stationToProperty )
  {
    final IValuePropertyType oldValue = m_stationToProperty;

    m_stationToProperty = stationToProperty;

    firePropertyChange( PROPERTY_STATION_TO_PROPERTY, oldValue, stationToProperty );
  }

  public boolean getUseKmValue( )
  {
    return m_useKmValue;
  }

  public void setUseKmValues( final boolean useKmValues )
  {
    final boolean oldValue = m_useKmValue;

    m_useKmValue = useKmValues;

    firePropertyChange( PROPERTY_USE_KM_VALUES, oldValue, useKmValues );
  }

  public int getSamplingDistance( )
  {
    return m_samplingDistance;
  }

  public void setSamplingDistance( final int samplingDistance )
  {
    final int oldValue = m_samplingDistance;

    m_samplingDistance = samplingDistance;

    firePropertyChange( PROPERTY_SAMPLING_DISTANCE, oldValue, samplingDistance );
  }

  public String[] getRiverNames( )
  {
    return m_riverNames;
  }

  public void setRiverNames( final String[] riverNames )
  {
    final String[] oldValue = riverNames;

    m_riverNames = riverNames;

    firePropertyChange( PROPERTY_RIVER_NAMES, oldValue, riverNames );
  }

  public String getSelectedRiverName( )
  {
    return m_selectedRiverName;
  }

  public void setSelectedRiverName( final String river )
  {
    final Object oldValue = m_selectedRiverName;

    m_selectedRiverName = river;

    firePropertyChange( PROPERTY_SELECTED_RIVER_NAME, oldValue, m_selectedRiverName );
  }

  protected void updateProperties( )
  {
    updateProperty( PROPERTY_RIVER_NAME_INPUT, PROPERTY_RIVER_NAME_PROPERTY, RIVER_NAME_FIELD, false, String.class, Number.class );
    updateProperty( PROPERTY_STATION_FROM_INPUT, PROPERTY_STATION_FROM_PROPERTY, FROM_STATION_FIELD, true, String.class, Number.class );
    updateProperty( PROPERTY_STATION_TO_INPUT, PROPERTY_STATION_TO_PROPERTY, TO_STATION_FIELD, true, String.class, Number.class );

    final String[] riverNames = readRiverNames();
    setRiverNames( riverNames );

    if( riverNames.length == 0 )
      setSelectedRiverName( null );
    else
      setSelectedRiverName( riverNames[0] );
  }

  private void updateProperty( final String inputProperty, final String propertyProperty, final String defaultProperty, final boolean optional, final Class< ? >... types )
  {
    final IValuePropertyType[] props = findThemeProperties( types );
    final IValuePropertyType[] input;
    if( optional )
      input = ArrayUtils.addAll( new IValuePropertyType[] { NO_RPOPERTY_SELECTED }, props );
    else
      input = props;

    BeanProperties.value( inputProperty ).setValue( this, input );

    final IValuePropertyType property = findPropertyToSet( input, defaultProperty );

    BeanProperties.value( propertyProperty ).setValue( this, property );
  }

  private IValuePropertyType findPropertyToSet( final IValuePropertyType[] input, final String defaultProperty )
  {
    if( input.length == 0 )
      return null;

    /* Search for default */
    for( final IValuePropertyType vpt : input )
    {
      if( vpt != null )
      {
        final String name = vpt.getQName().getLocalPart();
        if( name.equals( defaultProperty ) )
          return vpt;
      }
    }

    /* set to first */
    return input[0];
  }

  private IValuePropertyType[] findThemeProperties( final Class< ? >[] types )
  {
    if( m_riverLineTheme == null )
      return new IValuePropertyType[0];

    final Collection<IValuePropertyType> valueProperties = new LinkedHashSet<>();

    final IFeatureType featureType = getFeatureType();

    final IPropertyType[] properties = featureType.getProperties();
    for( final IPropertyType pt : properties )
    {
      if( pt instanceof IValuePropertyType )
      {
        final IValuePropertyType vpt = (IValuePropertyType) pt;
        if( pt.isList() || vpt.isGeometry() )
          continue;

        final Class< ? > valueClass = vpt.getValueClass();
        for( final Class< ? > type : types )
        {
          if( type.isAssignableFrom( valueClass ) )
            valueProperties.add( vpt );
        }
      }
    }

    return valueProperties.toArray( new IValuePropertyType[valueProperties.size()] );
  }

  private IFeatureType getFeatureType( )
  {
    // TRICKY: special case for shapes, because the target feature type is never the concrete feature type.
    final FeatureList featureList = m_riverLineTheme.getFeatureList();
    final Feature owner = featureList.getOwner();
    if( owner instanceof ShapeCollection && featureList.size() > 0 )
    {
      final Object object = featureList.get( 0 );
      final Feature firstFeature = FeatureHelper.getFeature( owner.getWorkspace(), object );
      return firstFeature.getFeatureType();
    }

    return m_riverLineTheme.getFeatureType();
  }

  private String[] readRiverNames( )
  {
    /* Names are unique and sorted alphabetically */
    final Set<String> names = new TreeSet<>();

    if( m_riverLineTheme != null && m_riverNameProperty != null )
    {
      final FeatureList featureList = m_riverLineTheme.getFeatureList();
      final GMLWorkspace workspace = featureList.getOwner().getWorkspace();
      for( final Object element : featureList )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, element );
        final String name = ObjectUtils.toString( feature.getProperty( m_riverNameProperty ) );
        names.add( name );
      }
    }

    return names.toArray( new String[names.size()] );
  }

  public Feature[] getSelectedRivers( )
  {
    if( m_riverLineTheme == null || m_selectedRiverName == null || m_riverNameProperty == null )
      return null;

    final Collection<Feature> foundFeatures = new ArrayList<>();

    final FeatureList featureList = m_riverLineTheme.getFeatureList();
    final GMLWorkspace workspace = featureList.getOwner().getWorkspace();
    for( final Object element : featureList )
    {
      final Feature feature = FeatureHelper.getFeature( workspace, element );
      final String name = ObjectUtils.toString( feature.getProperty( m_riverNameProperty ) );
      if( m_selectedRiverName.equals( name ) )
        foundFeatures.add( feature );
    }

    return foundFeatures.toArray( new Feature[foundFeatures.size()] );
  }

  public LengthSectionHandlerParameters getParameters( )
  {
    final BigDecimal samplingDistance = new BigDecimal( getSamplingDistance() );

    final IValuePropertyType stationFromProperty = getStationFromProperty();
    final IValuePropertyType stationToProperty = getStationToProperty();

    final IValuePropertyType fromProperty = stationFromProperty == NO_RPOPERTY_SELECTED ? null : stationFromProperty;
    final IValuePropertyType toProperty = stationToProperty == NO_RPOPERTY_SELECTED ? null : stationToProperty;

    return new LengthSectionHandlerParameters( getSelectedRivers(), getSelectedRiverName(), fromProperty, toProperty, samplingDistance, getUseKmValue() );
  }

  public IKalypsoFeatureTheme[] getRiverThemes( )
  {
    return m_lineThemes;
  }
}