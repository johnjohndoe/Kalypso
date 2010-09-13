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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.kalypso.afgui.model.Util;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Provide the implementation of {@link ITerrainElevationModel} for simBase:NativeTerrainElevationModelWrapper model.
 * This class collaborates with ...
 *
 * @author Madanagopal
 * @author Patrice Congo
 * 
 */
public class NativeTerrainElevationModelWrapper extends TerrainElevationModel implements INativeTerrainElevationModelWrapper
{
  public static final QName SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "NativeTerrainElevationModelWrapper" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_BASE_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "_TerrainElevationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_FILE_NAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "fileName" ); //$NON-NLS-1$

  private final IElevationProvider m_elevationProvider;

  private final IFile m_file;

  /**
   * Creates a new {@link NativeTerrainElevationModelWrapper} as child of the given parent and link to it by the prop of
   * the given name. The native source path is also set
   */
  private static final Feature createFeature( final Feature parentFeature, final QName propQName, String sourceName )
  {
    Assert.throwIAEOnNullParam( parentFeature, "parentFeature" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( propQName, "propQName" ); //$NON-NLS-1$
    sourceName = Assert.throwIAEOnNullOrEmpty( sourceName );

    final Feature newFeature = Util.createFeatureAsProperty( parentFeature, propQName, SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER, new Object[] { sourceName }, new QName[] { SIM_BASE_PROP_FILE_NAME } );

    // newFeature.setProperty(
    // KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_FILE_NAME, sourceName );

    return newFeature;
  }

  public NativeTerrainElevationModelWrapper( final ITerrainElevationModelSystem terrainElevationModelSystem, final String sourceName ) throws IllegalArgumentException, IOException, URISyntaxException
  {
    this( createFeatureForTEMSystem( terrainElevationModelSystem, sourceName ) );
  }

  private static final Feature createFeatureForTEMSystem( final ITerrainElevationModelSystem terrainElevationModelSystem, final String sourceName )
  {
    return createFeature( terrainElevationModelSystem.getFeature(), TerrainElevationModelSystem.SIM_BASE_PROP_TERRAIN_ELE_MODEL, sourceName );
  }

  public NativeTerrainElevationModelWrapper( final Feature featureToBind ) throws IllegalArgumentException, IOException, URISyntaxException
  {
    super( featureToBind, SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER );

    final String sourceName = (String) featureToBind.getProperty( SIM_BASE_PROP_FILE_NAME );
    if( sourceName == null )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper.2" ) ); //$NON-NLS-1$
    }

    // why using urls and file? We live in an eclipse workspace! Use IRources
    final URL sourceURL = makeSourceURL( sourceName, featureToBind.getWorkspace().getContext() );

    m_file = ResourceUtilities.findFileFromURL( sourceURL );
    m_elevationProvider = NativeTerrainElevationModelFactory.getTerrainElevationModel( m_file.getLocation().toFile() );
  }

  private final URL makeSourceURL( final String sourceName, final URL worspaceContex ) throws URISyntaxException, MalformedURLException
  {
    try
    {
      final URL sourceUrl = new URL( sourceName );
      final URI uri = sourceUrl.toURI();
      if( uri.isAbsolute() )
      {
        return uri.toURL();
      }
      else
      {
        final URL absURL = new URL( worspaceContex, sourceName );
        return absURL;
      }
    }
    catch( final MalformedURLException e )
    {
      // e.printStackTrace();
      return new URL( worspaceContex, sourceName );
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getElevation( final GM_Point location )
  {
    return m_elevationProvider.getElevation( location );
  }

  public IElevationProvider getElevationProvider( )
  {
    return m_elevationProvider;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  @Override
  public GM_Envelope getBoundingBox( )
  {
    return m_elevationProvider.getBoundingBox();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    return m_elevationProvider.getCoordinateSystem();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   */
  @Override
  public double getMaxElevation( )
  {
    return m_elevationProvider.getMaxElevation();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMinElevation()
   */
  @Override
  public double getMinElevation( )
  {
    return m_elevationProvider.getMinElevation();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper#getSourceFile()
   */
  @Override
  public IFile getSourceFile( )
  {
    return m_file;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String coordinateSystem )
  {
    // TODO

  }
}
