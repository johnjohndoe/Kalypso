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
package org.kalypso.model.hydrology.internal.postprocessing;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.timeseries.TSResultDescriptor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class DefaultPathGenerator
{
  public static String generateResultPathFor( final Feature feature, final TSResultDescriptor descriptor, final String extra )
  {
    final String extraString;
    if( extra == null )
      extraString = ""; //$NON-NLS-1$
    else
      extraString = extra;
    final String observationTitle = getObservationTitle( feature );

    final String directoryName = getDirectoryName( feature );
    return directoryName + '/' + observationTitle + extraString + '/' + descriptor.getResultFilename() + ".zml"; //$NON-NLS-1$
  }

  private static String getDirectoryName( final Feature feature )
  {
    final QName qName = feature.getFeatureType().getQName();
    if( Node.FEATURE_NODE.equals( qName ) )
      return RrmCalculationResult.FOLDER_NODE;

    if( StorageChannel.FEATURE_STORAGE_CHANNEL.equals( qName ) )
      return RrmCalculationResult.FOLDER_STROAGE_CHANNEL;

    if( Catchment.FEATURE_CATCHMENT.equals( qName ) )
      return RrmCalculationResult.FOLDER_CATCHMENT;

    throw new IllegalArgumentException( String.format( Messages.getString("DefaultPathGenerator.0"), qName ) ); //$NON-NLS-1$
  }

  private static String getObservationTitle( final Feature feature )
  {
    final String feName = feature.getName();
    if( StringUtils.isBlank( feName ) )
      return feature.getId();

    return feName;
  }

  public static String generateTitleForObservation( final Feature feature, final TSResultDescriptor descriptor )
  {
    final String observationTitle = getObservationTitle( feature );

    final String annotationName = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_NAME );

    return observationTitle + " - " + DefaultPathGenerator.getTitleForSuffix( descriptor ) + " " + annotationName; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public static String getTitleForSuffix( final TSResultDescriptor descriptor )
  {
    return descriptor.getAxisTitle();
  }
}
