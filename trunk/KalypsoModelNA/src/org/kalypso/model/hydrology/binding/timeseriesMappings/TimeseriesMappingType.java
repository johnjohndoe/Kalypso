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
package org.kalypso.model.hydrology.binding.timeseriesMappings;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

public enum TimeseriesMappingType
{
  gaugeMeasurement(Messages.getString( "TimeseriesMappingType_0" ), Node.FEATURE_NODE, ITimeseriesConstants.TYPE_DISCHARGE, Node.PROPERTY_PEGEL_ZR), //$NON-NLS-1$
  nodeInflow(Messages.getString( "TimeseriesMappingType_1" ), Node.FEATURE_NODE, ITimeseriesConstants.TYPE_DISCHARGE, Node.PROPERTY_ZUFLUSS_ZR), //$NON-NLS-1$
  storageEvaporation(
      Messages.getString( "TimeseriesMappingType_2" ), StorageChannel.FEATURE_STORAGE_CHANNEL, ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED, StorageChannel.PROPERTY_SEA_EVAPORATION_ZMLLINK); //$NON-NLS-1$

  private String m_label;

  private final QName m_elementType;

  private final String m_parameterType;

  private final QName m_modelLinkProperty;

  private TimeseriesMappingType( final String label, final QName elementType, final String parameterType, final QName modelLinkProperty )
  {
    m_label = label;
    m_elementType = elementType;
    m_parameterType = parameterType;
    m_modelLinkProperty = modelLinkProperty;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public Feature[] getModelElements( final NaModell naModel )
  {
    final IFeatureBindingCollection< ? extends Feature> modelElements = getModelElementList( naModel );
    final List<Feature> filteredElements = filterElements( modelElements );
    return filteredElements.toArray( new Feature[filteredElements.size()] );
  }

  private List<Feature> filterElements( final IFeatureBindingCollection< ? extends Feature> modelElements )
  {
    final List<Feature> result = new ArrayList<>();

    for( final Feature feature : modelElements )
    {
      if( feature.getFeatureType().getQName().equals( m_elementType ) )
        result.add( feature );
    }

    return result;
  }

  private IFeatureBindingCollection< ? extends Feature> getModelElementList( final NaModell naModel )
  {
    switch( this )
    {
      case gaugeMeasurement:
      case nodeInflow:
        return naModel.getNodes();

      case storageEvaporation:
        return naModel.getChannels();
    }

    throw new IllegalStateException();
  }

  public String getLinkParameterType( )
  {
    return m_parameterType;
  }

  public String getElementName( )
  {
    final IFeatureType featureType = GMLSchemaUtilities.getFeatureTypeQuiet( m_elementType );
    return featureType.getAnnotation().getValue( IAnnotation.ANNO_NAME );
  }

  public QName getModelLinkProperty( )
  {
    return m_modelLinkProperty;
  }

  public String getTargetFolderName( final RrmSimulation simulation )
  {
    switch( this )
    {
      case gaugeMeasurement:
        return simulation.getGaugeFolder().getName();

      case nodeInflow:
        return simulation.getNodeInflowFolder().getName();

      case storageEvaporation:
        // TODO: bad; sea evaporation does not belong into climate folder in my opinion
        return simulation.getClimateFolder().getName();
    }

    throw new IllegalArgumentException();
  }
}