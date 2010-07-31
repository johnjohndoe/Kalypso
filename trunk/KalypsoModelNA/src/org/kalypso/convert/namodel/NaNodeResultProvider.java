/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

package org.kalypso.convert.namodel;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlUtilities;
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

  private final List<Feature> removedResults = new ArrayList<Feature>();

  private final boolean m_useResults;

  public NaNodeResultProvider( final GMLWorkspace modellWorkspace, final GMLWorkspace controlWorkspace, final URL context )
  {
    m_context = context;
    final Feature controlFE = controlWorkspace.getRootFeature();
    m_useResults = FeatureHelper.booleanIsTrue( controlFE, NaModelConstants.NACONTROL_USE_RESULTS_PROP, true );
    final String resultNodeID = (String) controlFE.getProperty( NaModelConstants.NACONTROL_ROOTNODE_PROP );
    // exclude some node from providing results
    if( resultNodeID != null )
    {
      removeResult( modellWorkspace.getFeature( resultNodeID ) );
    }
    else
    {
      final IFeatureType nodeFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
      final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
      for( final Feature nodeFE : nodeFEs )
      {
        if( FeatureHelper.booleanIsTrue( nodeFE, NaModelConstants.GENERATE_RESULT_PROP, false ) )
          removeResult( nodeFE );
      }
    }
  }

  private URL getResultURL( final Feature nodeFE ) throws MalformedURLException
  {
    final TimeseriesLinkType link = (TimeseriesLinkType) nodeFE.getProperty( NaModelConstants.NODE_RESULT_TIMESERIESLINK_PROP );
    if( link == null )
      return null;
    // optionen loeschen
    final String href = link.getHref().replaceAll( "\\?.*", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    final IUrlResolver res = new UrlUtilities();
    return res.resolveURL( m_context, href );
  }

  public URL getMeasuredURL( final Feature nodeFE ) throws MalformedURLException
  {
    final TimeseriesLinkType link = (TimeseriesLinkType) nodeFE.getProperty( NaModelConstants.NODE_PEGEL_ZR_PROP );
    if( link == null )
      return null;
    // optionen loeschen
    final String href = link.getHref().replaceAll( "\\?.*", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    final IUrlResolver res = new UrlUtilities();
    return res.resolveURL( m_context, href );
  }

  public boolean resultExists( final Feature nodeFE )
  {
    if( !m_useResults )
      return false;
    if( removedResults.contains( nodeFE ) )
      return false;
    try
    {
      final URL resultURL = getResultURL( nodeFE );
      resultURL.openStream();
    }
    catch( final Exception e )
    {
      return false;
    }
    return true;
  }

  private void removeResult( final Feature nodeFE )
  {
    if( !removedResults.contains( nodeFE ) )
      removedResults.add( nodeFE );
  }
}
