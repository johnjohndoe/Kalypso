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
package org.kalypso.model.hydrology.internal;

import java.io.File;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.util.optimize.NaOptimizeLoader;
import org.kalypso.ogc.gml.serialize.FeatureProviderWithCacheFactory;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IFeatureProvider;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * @author Gernot Belger
 */
public class NaSimulationData implements INaSimulationData
{
  // REMARK: in order to support references between the models, we use this factory that makes sure, that no workspace
  // gets loaded twice.
  private final FeatureProviderWithCacheFactory m_factory = new FeatureProviderWithCacheFactory();

  private final GMLWorkspace m_parameterWorkspace;

  private final GMLWorkspace m_sudsWorkspace;

  private final GMLWorkspace m_synthNWorkspace;

  private final GMLWorkspace m_modelWorkspace;

  private final GMLWorkspace m_lzsimWorkspace;

  private final NAModellControl m_naModellControl;

  private final NAControl m_metaControl;

  private final NAHydrotop m_hydrotopeCollection;

  private final NaModell m_naModel;

  private NaOptimizeData m_optimizeData;

  private final URL m_preprocessedASCIIlocation;

  public NaSimulationData( final URL modelUrl, final URL controlURL, final URL metaUrl, final URL parameterUrl, final URL hydrotopUrl, final URL sudsUrl, final URL syntNUrl, final URL lzsimUrl, final NaOptimizeLoader optimizeLoader, final URL preprocessedASCIIlocation ) throws Exception
  {
    m_preprocessedASCIIlocation = preprocessedASCIIlocation;
    /*
     * Loading model workspace first, it is used as context for all other models (i.e. we assume they all live in the
     * same directory)
     */
    m_modelWorkspace = readWorkspaceOrNull( modelUrl );

    m_naModellControl = readModel( controlURL, NAModellControl.class );
    m_metaControl = readModel( metaUrl, NAControl.class );

    if( optimizeLoader == null )
      m_optimizeData = null;
    else
      m_optimizeData = optimizeLoader.load( m_modelWorkspace, m_factory );

    m_naModel = (NaModell) m_modelWorkspace.getRootFeature();

    m_parameterWorkspace = readWorkspaceOrNull( parameterUrl );

    m_hydrotopeCollection = readModel( hydrotopUrl, NAHydrotop.class );

    m_sudsWorkspace = loadAndCheckForFile( sudsUrl );
    m_lzsimWorkspace = loadAndCheckForFile( lzsimUrl );

    final String syntNpath = syntNUrl == null ? null : syntNUrl.getFile();
    final File syntNGML = syntNpath == null ? null : new File( syntNpath, "calcSynthN.gml" ); //$NON-NLS-1$
    m_synthNWorkspace = loadAndCheckForFile( syntNGML );

    if( m_hydrotopeCollection != null && m_modelWorkspace != null )
      transformModelToHydrotopeCrs();
  }

  private <T> T readModel( final URL location, final Class<T> type ) throws Exception
  {
    final GMLWorkspace workspace = readWorkspaceOrNull( location );
    if( workspace == null )
      return null;

    return type.cast( workspace.getRootFeature() );
  }

  private GMLWorkspace readWorkspaceOrNull( final URL location ) throws Exception
  {
    if( location == null )
      return null;

    final IFeatureProvider provider = m_factory.createFeatureProvider( m_modelWorkspace, location.toURI().toString() );
    return provider.getWorkspace();
  }

  @Override
  public void dispose( )
  {
    if( m_naModellControl != null )
      m_naModellControl.getWorkspace().dispose();

    if( m_parameterWorkspace != null )
      m_parameterWorkspace.dispose();

    if( m_sudsWorkspace != null )
      m_sudsWorkspace.dispose();

    if( m_synthNWorkspace != null )
      m_synthNWorkspace.dispose();

    if( m_modelWorkspace != null )
      m_modelWorkspace.dispose();

    if( m_lzsimWorkspace != null )
      m_lzsimWorkspace.dispose();

    if( m_naModellControl != null )
      m_naModellControl.getWorkspace().dispose();

    if( m_metaControl != null )
      m_metaControl.getWorkspace().dispose();

    if( m_hydrotopeCollection != null )
      m_hydrotopeCollection.getWorkspace().dispose();

    m_factory.dispose();
  }

  private void transformModelToHydrotopeCrs( )
  {
    final String targetCS = determineHydrotopeCrs();
    if( targetCS == null )
      return;

    final TransformVisitor visitor = new TransformVisitor( targetCS );
    m_modelWorkspace.accept( visitor, m_naModel, FeatureVisitor.DEPTH_INFINITE ); //$NON-NLS-1$
  }

  private String determineHydrotopeCrs( )
  {
    if( m_hydrotopeCollection == null )
      return null;

    // TODO: this is wrong: why transform the model to the hydrotope workspace?
    // Normally, we should transform both (better every loaded model) into the current kalypso crs
    final IFeatureBindingCollection<IHydrotope> hydrotopes = m_hydrotopeCollection.getHydrotopes();
    for( final IHydrotope hydrotop : hydrotopes )
    {
      final GM_Object geom = hydrotop.getGeometry();
      if( geom != null && geom.getCoordinateSystem() != null )
        return geom.getCoordinateSystem();
    }

    return null;
  }

  private GMLWorkspace loadAndCheckForFile( final URL location ) throws Exception
  {
    if( location == null )
      return null;

    final File file = FileUtils.toFile( location );
    /* Silently ignore if file does not exists */
    if( file != null && !file.exists() )
      return null;

    final IFeatureProvider provider = m_factory.createFeatureProvider( m_modelWorkspace, location.toURI().toString() );
    return provider.getWorkspace();
  }

  private GMLWorkspace loadAndCheckForFile( final File file ) throws Exception
  {
    if( file != null && file.exists() )
    {
      final String uri = file.toURI().toString();
      final IFeatureProvider provider = m_factory.createFeatureProvider( m_modelWorkspace, uri );
      return provider.getWorkspace();
    }

    return null;
  }

  @Override
  public NAModellControl getNaControl( )
  {
    return m_naModellControl;
  }

  @Override
  public GMLWorkspace getModelWorkspace( )
  {
    return m_modelWorkspace;
  }

  @Override
  public NaModell getNaModel( )
  {
    return m_naModel;
  }

  @Override
  public NAControl getMetaControl( )
  {
    return m_metaControl;
  }

  @Override
  public GMLWorkspace getParameterWorkspace( )
  {
    return m_parameterWorkspace;
  }

  @Override
  public GMLWorkspace getSudsWorkspace( )
  {
    return m_sudsWorkspace;
  }

  @Override
  public GMLWorkspace getSynthNWorkspace( )
  {
    return m_synthNWorkspace;
  }

  @Override
  public NAHydrotop getHydrotopCollection( )
  {
    return m_hydrotopeCollection;
  }

  @Override
  public InitialValues getInitialValues( )
  {
    if( m_lzsimWorkspace == null )
      return null;

    return (InitialValues) m_lzsimWorkspace.getRootFeature();
  }

  /**
   * @see org.kalypso.model.hydrology.INaSimulationData#getOptimizeData()
   */
  @Override
  public NaOptimizeData getOptimizeData( )
  {
    return m_optimizeData;
  }

  /**
   * @see org.kalypso.model.hydrology.INaSimulationData#getNaOptimize()
   */
  @Override
  public NAOptimize getNaOptimize( )
  {
    if( m_optimizeData == null )
      return null;

    return m_optimizeData.getNaOptimize();
  }

  /**
   * @see org.kalypso.model.hydrology.INaSimulationData#getPreprocessedASCII()
   */
  @Override
  public URL getPreprocessedASCII( )
  {
    return m_preprocessedASCIIlocation;
  }
}
