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
package org.kalypso.kalypso1d2d.pjt.map;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.FolderUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Thomas Jung
 * 
 */
public class HydrographUtils
{
  @SuppressWarnings("unchecked")
  public static GM_Position getHydroPositionFromElement( final IFeatureWrapper2 modelElement )
  {
    try
    {
      /* Node: return its position */
      final GM_Object geom;
      if( modelElement instanceof IFE1D2DNode )
        geom = ((IFE1D2DNode) modelElement).getPoint();
      else
        geom = null;

      if( geom != null )
        return geom.getCentroid().getPosition();
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }

    return null;
  }

  public static IHydrographCollection createHydrograph( ICalcUnitResultMeta calcUnitResult, final IFolder scenarioFolder ) throws Exception
  {
    /* create new hydrograph.gml */
    final Feature hydrographFeature = createNewHydrograph( calcUnitResult, scenarioFolder );
    final IHydrographCollection newHydrograph = (IHydrographCollection) hydrographFeature.getAdapter( IHydrographCollection.class );

    // set a name
    final String hydrographName = calcUnitResult.getName();
    newHydrograph.setName( hydrographName );

    setResultPaths( newHydrograph, calcUnitResult, DOCUMENTTYPE.nodes );

    /* create a resultMeta entry */
    // delete the prior entry
    IDocumentResultMeta resultMeta = calcUnitResult.getDocument( DOCUMENTTYPE.hydrograph );
    if( resultMeta != null )
      calcUnitResult.removeChild( resultMeta );

    calcUnitResult.addDocument( "Ganglinien", "Ganglinien des Teilmodells " + calcUnitResult.getName(), DOCUMENTTYPE.hydrograph, new Path( "hydrograph/hydrograph.gml" ), Status.OK_STATUS, null, null );

    return newHydrograph;
  }

  private static void setResultPaths( IHydrographCollection newHydrograph, ICalcUnitResultMeta calcUnitResult, DOCUMENTTYPE documenttype ) throws Exception
  {
    IDocumentResultMeta[] documents = calcUnitResult.getDocuments( documenttype );

    Map<IPath, Date> resultMap = new HashMap<IPath, Date>();

    for( IDocumentResultMeta documentResultMeta : documents )
    {
      IResultMeta parent = documentResultMeta.getParent();
      if( parent instanceof IStepResultMeta )
      {
        IStepResultMeta stepResult = (IStepResultMeta) parent;

        /* ignore steady and min / max results */
        if( stepResult.getStepType() == IStepResultMeta.STEPTYPE.unsteady )
          resultMap.put( documentResultMeta.getFullPath(), stepResult.getStepTime() );
      }
    }

    newHydrograph.setResults( resultMap );
  }

  public static IHydrographCollection getHydrograph( ICalcUnitResultMeta calcUnitResult, final IFolder scenarioFolder ) throws MalformedURLException, Exception
  {
    IDocumentResultMeta docResult;
    final IResultMeta child = calcUnitResult.getDocument( DOCUMENTTYPE.hydrograph );
    if( child != null )
    {

      // get the hydrograph
      docResult = (IDocumentResultMeta) child;
      IPath docPath = docResult.getFullPath();
      IFolder folder = scenarioFolder.getFolder( docPath );

      IFile file = folder.getFile( "" );
      if( !file.exists() )
      {
        // delete non-valid result meta entry
        calcUnitResult.removeChild( docResult );
        return null;
      }
      final URL hydrographURL = ResourceUtilities.createURL( folder );

      GMLWorkspace w = GmlSerializer.createGMLWorkspace( hydrographURL, null );

      final Feature hydroFeature = w.getRootFeature();
      return (IHydrographCollection) hydroFeature.getAdapter( IHydrographCollection.class );
    }
    return null;
  }

  public static Feature createNewHydrograph( ICalcUnitResultMeta calcUnitResult, final IFolder scenarioFolder ) throws CoreException, InvocationTargetException, GmlSerializeException, IOException
  {
    /* get a path */
    final IPath docPath = calcUnitResult.getFullPath().append( "hydrograph" );
    final IFolder calcUnitFolder = scenarioFolder.getFolder( docPath );
    if( !calcUnitFolder.exists() )
      FolderUtilities.mkdirs( calcUnitFolder );

    final IFile gmlResultFile = calcUnitFolder.getFile( "hydrograph.gml" );
    final URL url = ResourceUtilities.createURL( gmlResultFile );

    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( IHydrographCollection.QNAME, url, null );
    final Feature hydrographFeature = workspace.getRootFeature();

    OutputStreamWriter writer = null;
    try
    {
      writer = new OutputStreamWriter( new FileOutputStream( gmlResultFile.getLocation().toFile() ) );
      GmlSerializer.serializeWorkspace( writer, workspace, "UTF-8" );
      writer.close();

      // refresh workspace
      /* update resource folder */
      gmlResultFile.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );

      return hydrographFeature;
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  public static void setHydrographComponents( final Feature feature )
  {
    final List<String> componentUrnList = new ArrayList<String>();

    /* define hydrograph observation components */
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_DEPTH );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_VELOCITY );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );

    String[] componentUrns = componentUrnList.toArray( new String[componentUrnList.size()] );

    /* create the components */
    final IComponent[] components = new IComponent[componentUrns.length];

    for( int i = 0; i < components.length; i++ )
      components[i] = ObservationFeatureFactory.createDictionaryComponent( feature, componentUrns[i] );

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( feature );

    /* set to the results */
    final TupleResult result = obs.getResult();
    for( final IComponent component : components )
      result.addComponent( component );

  }

  public static void addHydrographTheme( IKalypsoLayerModell modell, final IHydrographCollection hydroCollection, ICalcUnitResultMeta calcResult ) throws Exception
  {
    { // Hydrograph
      final StyledLayerType hydroLayer = new StyledLayerType();
      String path = "../" + calcResult.getFullPath().toPortableString() + "/hydrograph/hydrograph.gml";

      hydroLayer.setName( "Ganglinienpunkte (" + hydroCollection.getName() + ")" );
      hydroLayer.setFeaturePath( "hydrographMember" );
      hydroLayer.setLinktype( "gml" );
      hydroLayer.setType( "simple" );
      hydroLayer.setVisible( true );
      hydroLayer.setActuate( "onRequest" );
      hydroLayer.setHref( path );
      hydroLayer.setVisible( true );
      final Property layerPropertyDeletable = new Property();
      layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
      layerPropertyDeletable.setValue( "true" );

      final Property layerPropertyThemeInfoId = new Property();
      layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
      layerPropertyThemeInfoId.setValue( "org.kalypso.ogc.gml.map.themeinfo.HydrographThemeInfo?format=Ganglinienpunkt (" + hydroCollection.getName() + ") %.2f" );

      final List<Property> layerPropertyList = hydroLayer.getProperty();
      layerPropertyList.add( layerPropertyDeletable );
      layerPropertyList.add( layerPropertyThemeInfoId );

      final List<Style> styleList = hydroLayer.getStyle();
      final Style style = new Style();
      style.setLinktype( "sld" );
      style.setStyle( "hydrographUserStyle" );
      style.setActuate( "onRequest" );
      style.setHref( styleLocationForHydrograph() );
      style.setType( "simple" );
      styleList.add( style );

      modell.addLayer( hydroLayer );
    }
  }

  private static String styleLocationForHydrograph( )
  {
    return "../styles/hydrograph/hydrograph.sld";
  }
}
