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

import java.io.IOException;
import java.io.OutputStreamWriter;
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
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.FolderUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Thomas Jung
 *
 */
public class HydrographUtils
{
  public static GM_Position getHydroPositionFromElement( final Feature modelElement )
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

  public static IHydrographCollection createHydrograph( final ICalcUnitResultMeta calcUnitResult, final IFolder scenarioFolder ) throws Exception
  {
    /* create new hydrograph.gml */
    final Feature hydrographFeature = createNewHydrograph( calcUnitResult, scenarioFolder );
    final IHydrographCollection newHydrograph = (IHydrographCollection) hydrographFeature.getAdapter( IHydrographCollection.class );

    // set a name
    final String hydrographName = calcUnitResult.getName();
    hydrographFeature.setName( hydrographName );

    setResultPaths( newHydrograph, calcUnitResult, DOCUMENTTYPE.nodes );

    /* create a resultMeta entry */
    // delete the prior entry
    final IDocumentResultMeta resultMeta = calcUnitResult.getDocument( DOCUMENTTYPE.hydrograph );
    if( resultMeta != null )
      calcUnitResult.removeChild( resultMeta );

    ResultMeta1d2dHelper.addDocument( calcUnitResult, Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographUtils.0" ), Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographUtils.1" ) + calcUnitResult.getName(), DOCUMENTTYPE.hydrograph, new Path( "hydrograph/hydrograph.gml" ), Status.OK_STATUS, null, null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    return newHydrograph;
  }

  private static void setResultPaths( final IHydrographCollection newHydrograph, final ICalcUnitResultMeta calcUnitResult, final DOCUMENTTYPE documenttype ) throws Exception
  {
    final IDocumentResultMeta[] documents = calcUnitResult.getDocuments( documenttype );

    final Map<IPath, Date> resultMap = new HashMap<>();

    for( final IDocumentResultMeta documentResultMeta : documents )
    {
      final IResultMeta parent = documentResultMeta.getOwner();
      if( parent instanceof IStepResultMeta )
      {
        final IStepResultMeta stepResult = (IStepResultMeta) parent;

        /* ignore steady and min / max results */
        if( stepResult.getStepType() == IStepResultMeta.STEPTYPE.unsteady )
          resultMap.put( documentResultMeta.getFullPath(), stepResult.getStepTime() );
      }
    }

    newHydrograph.setResults( resultMap );
  }

  public static IHydrographCollection getHydrograph( final ICalcUnitResultMeta calcUnitResult, final IFolder scenarioFolder ) throws MalformedURLException, Exception
  {
    IDocumentResultMeta docResult;
    final IResultMeta child = calcUnitResult.getDocument( DOCUMENTTYPE.hydrograph );
    if( child != null )
    {
      // get the hydrograph
      docResult = (IDocumentResultMeta) child;
      final IPath docPath = docResult.getFullPath();

      final URL scenarioURL = ResourceUtilities.createURL( scenarioFolder );
      final URL hydrographURL = UrlUtilities.resolveWithZip( scenarioURL, docPath.toPortableString() );

      // TODO: resolve file from docpath
      // TODO: maybe this check will fail, if it is a zip file!
      final IFile file = scenarioFolder.getFile( docPath );
      if( !file.exists() )
      {
        // delete non-valid result meta entry
        calcUnitResult.removeChild( docResult );
        return null;
      }

      // final URL hydrographURL = ResourceUtilities.createURL( folder );

      final GMLWorkspace w = GmlSerializer.createGMLWorkspace( hydrographURL, null );

      final Feature hydroFeature = w.getRootFeature();
      return (IHydrographCollection) hydroFeature.getAdapter( IHydrographCollection.class );
    }
    return null;
  }

  public static IHydrograph createNewHydrographFeature( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation, final String name, final String description )
  {
    final IFeatureType newFT = GMLSchemaUtilities.getFeatureTypeQuiet( IHydrograph.QNAME );
    final Feature newFeature = workspace.createFeature( parentFeature, parentRelation, newFT );

    /* set the observation components */
    HydrographUtils.setHydrographComponents( newFeature );

    final IHydrograph hydrograph = (IHydrograph) newFeature.getAdapter( IHydrograph.class );
    hydrograph.setName( name );
    hydrograph.setDescription( description );

    return hydrograph;
  }

  // FIXME: handle saving via pool
  public static Feature createNewHydrograph( final ICalcUnitResultMeta calcUnitResult, final IFolder scenarioFolder ) throws CoreException, GmlSerializeException, IOException, GMLSchemaException
  {
    /* get a path */
    final IPath docPath = calcUnitResult.getFullPath().append( "hydrograph" ); //$NON-NLS-1$
    final IFolder calcUnitFolder = scenarioFolder.getFolder( docPath );
    if( !calcUnitFolder.exists() )
      FolderUtilities.mkdirs( calcUnitFolder );

    final IFile gmlResultFile = calcUnitFolder.getFile( "hydrograph.gml" ); //$NON-NLS-1$
    final URL url = ResourceUtilities.createURL( gmlResultFile );

    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( IHydrographCollection.QNAME, url, null );
    final Feature hydrographFeature = workspace.getRootFeature();

    final OutputStreamWriter writer = null;
    try
    {
      final String charset = "UTF-8"; //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( gmlResultFile.getLocation().toFile(), workspace, charset );
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
    final List<String> componentUrnList = new ArrayList<>();

    /* define hydrograph observation components */
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_DEPTH );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_VELOCITY );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_VELOCITY_DIRECTION );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_PER );
    componentUrnList.add( Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DIR );

    final String[] componentUrns = componentUrnList.toArray( new String[componentUrnList.size()] );

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

  public static void addHydrographTheme( final IKalypsoLayerModell modell, final IHydrographCollection hydroCollection, final ICalcUnitResultMeta calcResult ) throws Exception
  {
    { // Hydrograph
      final StyledLayerType hydroLayer = new StyledLayerType();
      final String path = "../" + calcResult.getFullPath().toPortableString() + "/hydrograph/hydrograph.gml"; //$NON-NLS-1$ //$NON-NLS-2$

      hydroLayer.setName( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographUtils.7", hydroCollection.getName() ) ); //$NON-NLS-1$
      hydroLayer.setFeaturePath( "hydrographMember" ); //$NON-NLS-1$
      hydroLayer.setLinktype( "gml" ); //$NON-NLS-1$
      hydroLayer.setType( "simple" ); //$NON-NLS-1$
      hydroLayer.setVisible( true );
      hydroLayer.setActuate( "onRequest" ); //$NON-NLS-1$
      hydroLayer.setHref( path );
      hydroLayer.setVisible( true );
      final Property layerPropertyDeletable = new Property();
      layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
      layerPropertyDeletable.setValue( "true" ); //$NON-NLS-1$

      final Property layerPropertyThemeInfoId = new Property();
      layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
      layerPropertyThemeInfoId.setValue( "org.kalypso.ogc.gml.map.themeinfo.HydrographThemeInfo?format=Ganglinienpunkt (" + hydroCollection.getName() + Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographUtils.15" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      final List<Property> layerPropertyList = hydroLayer.getProperty();
      layerPropertyList.add( layerPropertyDeletable );
      layerPropertyList.add( layerPropertyThemeInfoId );

      final List<Style> styleList = hydroLayer.getStyle();
      final Style style = new Style();
      style.setLinktype( "sld" ); //$NON-NLS-1$
      style.setStyle( "hydrographUserStyle" ); //$NON-NLS-1$
      style.setActuate( "onRequest" ); //$NON-NLS-1$
      style.setHref( styleLocationForHydrograph() );
      style.setType( "simple" ); //$NON-NLS-1$
      styleList.add( style );

      modell.addLayer( hydroLayer );
    }
  }

  private static String styleLocationForHydrograph( )
  {
    return "../styles/hydrograph/hydrograph.sld"; //$NON-NLS-1$
  }
}
