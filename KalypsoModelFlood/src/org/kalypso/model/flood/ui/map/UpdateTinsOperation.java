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
package org.kalypso.model.flood.ui.map;

import java.io.File;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.PropertiesUtilities;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.conv.results.TinResultWriter;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.binding.ITinReference.SOURCETYPE;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree.model.geometry.MinMaxSurfacePatchVisitor;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.hmo.HMOReader;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;

/**
 * TODO: remove the TriangulatedSurfaceTriangleEater from here: we do not want to have a dependency on 1d2d!<br>
 * 
 * Updates the data of some tin-references. I.e. re-reads the original tin and copies the data into the reference.
 * 
 * @author Gernot Belger
 * 
 */
public class UpdateTinsOperation implements ICoreRunnableWithProgress
{
  private final ITinReference[] m_tinReferences;

  private final SzenarioDataProvider m_provider;

  public UpdateTinsOperation( final ITinReference[] tinReferences, final SzenarioDataProvider provider )
  {
    m_tinReferences = tinReferences;
    m_provider = provider;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.0" ), m_tinReferences.length ); //$NON-NLS-1$
    try
    {
      for( final ITinReference ref : m_tinReferences )
        updateTinReference( ref, progress.newChild( 1, SubMonitor.SUPPRESS_NONE ) );
    }
    catch( final CoreException ce )
    {
      throw ce;
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      progress.done();
    }

    return Status.OK_STATUS;
  }

  private IStatus updateTinReference( final ITinReference ref, final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( ref.getName(), 100 );

    /* read source data */
    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.1" ) ); //$NON-NLS-1$

    final URL sourceLocation = ref.getSourceLocation();
    final Properties properties = PropertiesUtilities.collectProperties( sourceLocation.getQuery(), "&", "=", null ); //$NON-NLS-1$ //$NON-NLS-2$
    final SOURCETYPE sourceType = ref.getSourceType();
    final Date sourceDate = new Date();

    final MinMaxSurfacePatchVisitor<GM_Triangle> minmaxVisitor = new MinMaxSurfacePatchVisitor<GM_Triangle>();
    String desc;
    String crs;
    GM_TriangulatedSurface gmSurface = null;
    TriangulatedSurfaceTriangleEater eater;
    GMLWorkspace sourceWorkspace;

    final IProgressMonitor subMon = new SubProgressMonitor( monitor, 33 );
    switch( sourceType )
    {
      case gml:

        final GMLXPath sourcePath = ref.getSourceFeaturePath();

        // REMARK 1: loads the source tin directly into memory.... will bring performance problems...
        sourceWorkspace = GmlSerializer.createGMLWorkspace( sourceLocation, null );

        final Object sourceObject = sourcePath == null ? sourceWorkspace : GMLXPathUtilities.query( sourcePath, sourceWorkspace );
        final GM_TriangulatedSurface surface = findSurface( sourceObject );
        if( surface == null )
        {
          final String msg = Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.4", sourcePath, sourceObject ); //$NON-NLS-1$
          throw new CoreException( StatusUtilities.createErrorStatus( msg ) );
        }

        ProgressUtilities.worked( monitor, 33 );

        /* update target data */
        monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.5" ) ); //$NON-NLS-1$
        ref.setTin( null ); // reset tin to null before cloning the source in order to free some memory

        // REMARK 2: cloning the complete tin will result in performance problems...
        final GM_TriangulatedSurface clonedSurface = (GM_TriangulatedSurface) surface.clone();
        ProgressUtilities.worked( monitor, 33 );

        monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.6" ) ); //$NON-NLS-1$
        clonedSurface.acceptSurfacePatches( clonedSurface.getEnvelope(), minmaxVisitor, subMon );

        desc = Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.7", sourceLocation.toExternalForm(), sourcePath, sourceDate ); //$NON-NLS-1$

        ref.setDescription( desc );
        ref.setMin( minmaxVisitor.getMin() );
        ref.setMax( minmaxVisitor.getMax() );
        ref.setUpdateDate( sourceDate );
        ref.setTin( clonedSurface );

        break;

      case hmo:
        crs = getCoordinateSytem( properties.getProperty( "srs" ) ); //$NON-NLS-1$
        eater = new TriangulatedSurfaceTriangleEater( KalypsoDeegreePlugin.getDefault().getCoordinateSystem(), new TinResultWriter.QNameAndString[] {} );

        final IGeoTransformer transformer = GeoTransformerFactory.getGeoTransformer( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

        final URL hmoLocation = new URL( sourceLocation.getProtocol() + ":" + sourceLocation.getPath() ); //$NON-NLS-1$

        final HMOReader hmoReader = new HMOReader( new GeometryFactory() );
        final Reader r = new InputStreamReader( hmoLocation.openStream() );
        final LinearRing[] rings = hmoReader.read( r );

        int count = 0;
        for( final LinearRing ring : rings )
        {
          count++;
          final List<GM_Point> pointList = new LinkedList<GM_Point>();
          monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.10" ) + count + " / " + rings.length + "... " ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          for( int i = 0; i < ring.getNumPoints() - 1; i++ )
          {
            final GM_Object object = JTSAdapter.wrap( ring.getPointN( i ) );

            final GM_Point point = (GM_Point) object;
            point.setCoordinateSystem( crs );

            pointList.add( (GM_Point) transformer.transform( point ) );
          }
          eater.addPoints( pointList );
        }

        gmSurface = eater.getSurface();
        ProgressUtilities.worked( monitor, 33 );
        monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.13" ) ); //$NON-NLS-1$

        gmSurface.acceptSurfacePatches( gmSurface.getEnvelope(), minmaxVisitor, subMon );

        // TODO: check for right time zone?
        desc = Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.14", sourceLocation.toExternalForm(), sourceDate ); //$NON-NLS-1$

        ref.setDescription( desc );
        ref.setMin( minmaxVisitor.getMin() );
        ref.setMax( minmaxVisitor.getMax() );
        // TODO: check for right time zone?
        ref.setUpdateDate( sourceDate );
        ref.setTin( gmSurface );

        break;

      case shape:

        // open shape
        crs = getCoordinateSytem( properties.getProperty( "srs" ) ); //$NON-NLS-1$

        eater = new TriangulatedSurfaceTriangleEater( crs, new TinResultWriter.QNameAndString[] {} );

        final URL shapeURL = new URL( sourceLocation.getProtocol() + ":" + sourceLocation.getPath() ); //$NON-NLS-1$
        final String file2 = shapeURL.getFile();
        final File file = new File( file2 );
        // final IFile file = ResourceUtilities.findFileFromURL( shapeURL );
        final String absolutePath = file.getAbsolutePath();
        final String shapeBase = FileUtilities.nameWithoutExtension( absolutePath );

        // TODO:check shape type (at first only triangle polygonz are supported

        try
        {
          sourceWorkspace = ShapeSerializer.deserialize( shapeBase, crs );

          final Feature fRoot = sourceWorkspace.getRootFeature();
          final FeatureList lstMembers = (FeatureList) fRoot.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

          final GM_Object geom = ((Feature) lstMembers.get( 0 )).getDefaultGeometryProperty();
          gmSurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( crs );

          if( geom instanceof GM_MultiSurface )
          {
            // conversion
            // convert the gm_surfaces.exterior rings into gm.triangle
            for( final Object object : lstMembers )
            {
              if( object instanceof Feature )
              {
                final Feature feat = (Feature) object;
                final GM_Object[] geometryProperties = feat.getGeometryProperties();
                if( geometryProperties[0] instanceof GM_MultiSurface )
                {
                  final GM_MultiSurface polygonSurface = (GM_MultiSurface) geometryProperties[0];
                  final GM_Triangle[] triangles = ConstraintDelaunayHelper.convertToTriangles( polygonSurface, crs );

                  // add the triangles into the gm_triang_surfaces
                  for( final GM_Triangle element : triangles )
                  {
                    GM_Triangle triangle;
                    triangle = element;
                    gmSurface.add( triangle );
                    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.17" ) + gmSurface.size() ); //$NON-NLS-1$
                  }
                }
              }
            }
          }

          monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.18" ) ); //$NON-NLS-1$

          gmSurface.acceptSurfacePatches( gmSurface.getEnvelope(), minmaxVisitor, subMon );
          // TODO: check for right time zone?
          desc = Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.19", sourceLocation.toExternalForm(), sourceDate ); //$NON-NLS-1$

          ref.setDescription( desc );
          ref.setMin( minmaxVisitor.getMin() );
          ref.setMax( minmaxVisitor.getMax() );
          // TODO: check for right time zone?
          ref.setUpdateDate( sourceDate );
          ref.setTin( gmSurface );
        }
        catch( final GmlSerializeException e )
        {
          e.printStackTrace();
          StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.20" ) ); //$NON-NLS-1$
        }

        break;

    }

    /* Fire modell event as feature was changed */
    final Feature refFeature = ref.getFeature();
    final GMLWorkspace workspace = refFeature.getWorkspace();
    final ModellEvent event = new FeaturesChangedModellEvent( workspace, new Feature[] { refFeature } );
    workspace.fireModellEvent( event );

    /* post command in order to make the pool dirty */
    m_provider.postCommand( IFloodModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

    monitor.done();
    return Status.OK_STATUS;
  }

  private GM_TriangulatedSurface findSurface( final Object sourceObject )
  {
    if( sourceObject == null )
      return null;

    if( sourceObject instanceof GM_TriangulatedSurface )
      return (GM_TriangulatedSurface) sourceObject;

    /* Check, if it is a feature or take rootFeature if it is a workspace */
    final Feature feature;
    if( sourceObject instanceof Feature )
      feature = (Feature) sourceObject;
    else if( sourceObject instanceof GMLWorkspace )
      feature = ((GMLWorkspace) sourceObject).getRootFeature();
    else
      feature = null;

    if( feature == null )
      return null;

    /* Get first tin we find as property */
    final IPropertyType[] properties = feature.getFeatureType().getProperties();
    for( final IPropertyType pt : properties )
    {
      final Object property = feature.getProperty( pt );
      if( property instanceof GM_TriangulatedSurface )
        return (GM_TriangulatedSurface) property;
    }

    return null;
  }

  protected static String getCoordinateSytem( final String crsName )
  {
    return crsName;
  }
}