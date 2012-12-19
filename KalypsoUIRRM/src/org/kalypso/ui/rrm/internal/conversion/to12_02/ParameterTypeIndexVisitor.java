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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.IStorageChannel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.conversion.ITimeseriesVisitor;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class ParameterTypeIndexVisitor implements ITimeseriesVisitor, IParameterTypeIndex
{
  private final Map<String, String> m_index = new HashMap<>();

  private final File m_oldModelDir;

  public ParameterTypeIndexVisitor( final File oldModelDir )
  {
    m_oldModelDir = oldModelDir;
  }

  @Override
  public IStatus visit( final Feature feature, final QName linkProperty )
  {
    final String parameterType = guessType( linkProperty );
    if( parameterType == null )
    {
      /* We only index specific cases */
      return Status.OK_STATUS;
    }

    /* Find relative path to file */
    final ZmlLink zmlLink = new ZmlLink( feature, linkProperty );

    final String href = zmlLink.getHref();
    if( StringUtils.isEmpty( href ) )
      return Status.OK_STATUS;

    final File zmlFile = resolveHref( href );

    /* Make relative to old timeserie folder */
    final File timeseriesDir = new File( m_oldModelDir, INaProjectConstants.FOLDER_ZEITREIHEN );
    final String relativeZmlPath = FileUtilities.getRelativePathTo( timeseriesDir, zmlFile );

    m_index.put( relativeZmlPath, parameterType );

    return Status.OK_STATUS;
  }

  private File resolveHref( final String href )
  {
    if( href.startsWith( UrlResolver.PROJECT_PROTOCOLL ) )
    {
      /* Is a path relative to the project */
      final String relPath = href.substring( UrlResolver.PROJECT_PROTOCOLL.length() + 1 ); //$NON-NLS-1$
      return new File( m_oldModelDir, relPath );
    }

    /* Must be a path relative to the timeseries folder */
    final File timeseriesDir = new File( m_oldModelDir, INaProjectConstants.FOLDER_ZEITREIHEN );
    return new File( timeseriesDir, href );
  }

  private String guessType( final QName linkProperty )
  {
    // REMARK: only handle those cases where we need to change the parameter type later on

// if( Node.PROPERTY_PEGEL_ZR.equals( linkProperty ) )
// return ITimeseriesConstants.TYPE_MEAN_EVAPORATION;

// if( Node.PROPERTY_RESULT_TIMESERIESLINK.equals( linkProperty ) )
// return ITimeseriesConstants.TYPE_MEAN_EVAPORATION;

// if( Node.PROPERTY_ZUFLUSS_ZR.equals( linkProperty ) )
// return ITimeseriesConstants.TYPE_MEAN_EVAPORATION;

// if( Node.PROPERTY_RESULT_AS_INFLOW_ZR.equals( linkProperty ) )
// return ITimeseriesConstants.TYPE_MEAN_EVAPORATION;

// if( Catchment.PROP_PRECIPITATION_LINK.equals( linkProperty ) )
// return ITimeseriesConstants.TYPE_MEAN_EVAPORATION;

    if( Catchment.PROP_TEMPERATURE_LINK.equals( linkProperty ) )
      return ITimeseriesConstants.TYPE_MEAN_TEMPERATURE;

    if( Catchment.PROP_EVAPORATION_LINK.equals( linkProperty ) )
      return ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED;

    if( IStorageChannel.PROPERTY_SEA_EVAPORATION_ZMLLINK.equals( linkProperty ) )
      return ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED;

    return null;
  }

  @Override
  public String getParameterType( final String zmlRelativePath )
  {
    return m_index.get( zmlRelativePath );
  }
}