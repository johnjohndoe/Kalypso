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
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.optimize.CalibrationConfig;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.optimize.transform.ParameterOptimizeContext;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.w3c.dom.Document;

/**
 * @author Gernot Belger
 */
public class NaSimulationData
{
  private final GMLWorkspace m_parameterWorkspace;

  private final GMLWorkspace m_sudsWorkspace;

  private final GMLWorkspace m_synthNWorkspace;

  private final GMLWorkspace m_modelWorkspace;

  private final GMLWorkspace m_lzsimWorkspace;

  private final NAModellControl m_naModellControl;

  private final NAControl m_metaControl;

  private final NAHydrotop m_hydrotopeCollection;

  private final NaModell m_naModel;

  public NaSimulationData( final URL modelUrl, final URL controlURL, final URL metaUrl, final URL parameterUrl, final URL hydrotopUrl, final URL sudsUrl, final URL syntNUrl, final URL lzsimUrl ) throws Exception
  {
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlURL, null );
    m_naModellControl = (NAModellControl) controlWorkspace.getRootFeature();

    m_modelWorkspace = loadModelWorkspace( modelUrl );
    m_naModel = (NaModell) m_modelWorkspace.getRootFeature();

    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( metaUrl, null );
    m_metaControl = (NAControl) metaWorkspace.getRootFeature();

    m_parameterWorkspace = GmlSerializer.createGMLWorkspace( parameterUrl, null );
    // FIXME: do not load hydrotopes, if preprocessed files exist
    final GMLWorkspace hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopUrl, null );
    m_hydrotopeCollection = (NAHydrotop) hydrotopWorkspace.getRootFeature();

    m_sudsWorkspace = loadAndCheckForFile( sudsUrl );
    m_lzsimWorkspace = loadAndCheckForFile( lzsimUrl );

    final String syntNpath = syntNUrl == null ? null : syntNUrl.getFile();
    final File syntNGML = syntNpath == null ? null : new File( syntNpath, "calcSynthN.gml" ); //$NON-NLS-1$
    if( syntNGML != null && syntNGML.exists() )
      m_synthNWorkspace = GmlSerializer.createGMLWorkspace( syntNGML, null );
    else
      m_synthNWorkspace = null;

    transformModelToHydrotopeCrs();
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

  private GMLWorkspace loadModelWorkspace( final URL modelUrl ) throws SimulationException
  {
    // Apply optimization parameters
    // TODO: does not really belong here, shouldn't the optimize job do this?
    final CalibrationConfig config = new CalibrationConfig( m_naModellControl );
    final ParameterOptimizeContext[] calContexts = config.getCalContexts();

    try
    {
      final Document modelDoc = XMLHelper.getAsDOM( modelUrl, true );

      OptimizeModelUtils.initializeModel( modelDoc, calContexts );

      return GmlSerializer.createGMLWorkspace( modelDoc, modelUrl, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "Failed to initialize model workspace", e );
    }
  }

  private static GMLWorkspace loadAndCheckForFile( final URL location ) throws Exception
  {
    if( location == null )
      return null;

    final File sudsFile = FileUtils.toFile( location );
    if( sudsFile == null )
      return GmlSerializer.createGMLWorkspace( location, null );

    /* Silently ignore if file does not exists */
    if( !sudsFile.exists() )
      return null;

    return GmlSerializer.createGMLWorkspace( location, null );
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

}
