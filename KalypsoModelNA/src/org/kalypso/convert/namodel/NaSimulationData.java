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
package org.kalypso.convert.namodel;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.convert.namodel.optimize.NaOptimizeLoader;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.ogc.gml.serialize.FeatureProviderWithCacheFactory;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IFeatureProvider;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.w3c.dom.Node;

/**
 * @author Gernot Belger
 */
public class NaSimulationData
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

  private final NaOptimizeLoader m_optimizeLoader;

  public static final NaSimulationData load( final ISimulationDataProvider inputProvider ) throws Exception
  {
    final URL modelUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    final URL controlURL = (URL) inputProvider.getInputForID( NaModelConstants.IN_CONTROL_ID );
    final URL metaUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_META_ID );
    final URL parameterUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    final URL hydrotopUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID );
    final URL syntNUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_RAINFALL_ID );
    final URL lzsimUrl = getStartConditionFile( inputProvider );
    final URL sudsUrl = getInputOrNull( inputProvider, NaModelConstants.IN_SUDS_ID );

    final NaOptimizeLoader optimizeLoader = new NaOptimizeLoader( inputProvider );

    return new NaSimulationData( modelUrl, controlURL, metaUrl, parameterUrl, hydrotopUrl, sudsUrl, syntNUrl, lzsimUrl, optimizeLoader );
  }

  public static URL getStartConditionFile( final ISimulationDataProvider inputProvider ) throws SimulationException
  {
    if( !inputProvider.hasID( NaModelConstants.IN_LZSIM_IN_ID ) )
      return null;

    final URL iniValuesFolderURL = (URL) inputProvider.getInputForID( NaModelConstants.IN_LZSIM_IN_ID );
    try
    {
      // TODO: crude way to create the new URL, necessary as probably we do not have a '/' at the end of the path
      return new URL( iniValuesFolderURL.toExternalForm() + "/lzsim.gml" ); //$NON-NLS-1$
    }
    catch( final MalformedURLException e )
    {
      throw new SimulationException( "Failed to read start condition file", e );
    }
  }

  private static URL getInputOrNull( final ISimulationDataProvider inputProvider, final String id ) throws SimulationException
  {
    if( !inputProvider.hasID( id ) )
      return null;

    return (URL) inputProvider.getInputForID( id );
  }

  public NaSimulationData( final URL modelUrl, final URL controlURL, final URL metaUrl, final URL parameterUrl, final URL hydrotopUrl, final URL sudsUrl, final URL syntNUrl, final URL lzsimUrl, final NaOptimizeLoader optimizeLoader ) throws Exception
  {
    /*
     * Loading model workspace first, it is used as context for all other models (i.e. we assume they all live in the
     * same directory)
     */
    m_modelWorkspace = readWorkspaceOrNull( modelUrl );

    m_naModellControl = readModel( controlURL, NAModellControl.class );
    m_metaControl = readModel( metaUrl, NAControl.class );

    m_optimizeLoader = optimizeLoader;
    if( m_optimizeLoader != null )
      m_optimizeLoader.load( m_modelWorkspace, m_factory );

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

  public NAModellControl getNaControl( )
  {
    return m_naModellControl;
  }

  public GMLWorkspace getModelWorkspace( )
  {
    return m_modelWorkspace;
  }

  public NaModell getNaModel( )
  {
    return m_naModel;
  }

  public NAControl getMetaControl( )
  {
    return m_metaControl;
  }

  public GMLWorkspace getParameterWorkspace( )
  {
    return m_parameterWorkspace;
  }

  public GMLWorkspace getSudsWorkspace( )
  {
    return m_sudsWorkspace;
  }

  public GMLWorkspace getSynthNWorkspace( )
  {
    return m_synthNWorkspace;
  }

  public NAHydrotop getHydrotopCollection( )
  {
    return m_hydrotopeCollection;
  }

  public InitialValues getInitialValues( )
  {
    if( m_lzsimWorkspace == null )
      return null;

    return (InitialValues) m_lzsimWorkspace.getRootFeature();
  }

  public NAOptimize getNaOptimize( )
  {
    if( m_optimizeLoader == null )
      return null;

    return m_optimizeLoader.getNaOptimize();
  }

  public Node getNaOptimizeDom( )
  {
    if( m_optimizeLoader == null )
      return null;

    return m_optimizeLoader.getOptimizeDom();
  }

}
