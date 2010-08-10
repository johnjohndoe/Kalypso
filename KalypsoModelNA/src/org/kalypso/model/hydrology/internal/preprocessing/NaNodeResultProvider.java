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

package org.kalypso.model.hydrology.internal.preprocessing;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.LinkedHashSet;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class NaNodeResultProvider
{
  private final URL m_context;

  private final Collection<Feature> m_removedResults = new LinkedHashSet<Feature>();

  private final boolean m_useResults;

  public NaNodeResultProvider( final GMLWorkspace modellWorkspace, final boolean useResults, final String rootNodeID, final URL zmlContext )
  {
    m_context = zmlContext;
    m_useResults = useResults;

    if( rootNodeID != null )
    {
      // The root node may never be used as results
      removeResult( modellWorkspace.getFeature( rootNodeID ) );
    }
    else
    {
      // Remove all nodes, that have 'isResult' checked
      final IFeatureType nodeFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
      final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
      for( final Feature nodeFE : nodeFEs )
      {
        if( FeatureHelper.booleanIsTrue( nodeFE, NaModelConstants.GENERATE_RESULT_PROP, false ) )
          removeResult( nodeFE );
      }
    }
  }

  private void removeResult( final Feature nodeFE )
  {
    m_removedResults.add( nodeFE );
  }

  public boolean checkResultExists( final Feature nodeFE )
  {
    if( !m_useResults )
      return false;

    if( m_removedResults.contains( nodeFE ) )
      return false;

    try
    {
      final URL resultURL = getResultURL( nodeFE );
      if( resultURL == null )
        return false;

      return FileUtilities.checkIsAccessible( resultURL );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      return false;
    }
  }

  private URL getResultURL( final Feature nodeFE ) throws MalformedURLException
  {
    final TimeseriesLinkType link = (TimeseriesLinkType) nodeFE.getProperty( NaModelConstants.NODE_RESULT_TIMESERIESLINK_PROP );
    if( link == null )
      return null;

    // delete query part
    final String href = link.getHref().replaceAll( "\\?.*", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    return new URL( m_context, href );
  }
}
