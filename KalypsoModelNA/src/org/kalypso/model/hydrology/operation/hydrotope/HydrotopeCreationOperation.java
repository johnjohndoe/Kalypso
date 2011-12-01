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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.Geology;
import org.kalypso.model.hydrology.binding.Hydrotop;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.SoilType;
import org.kalypso.model.hydrology.binding.suds.ISuds;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiPrimitive;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Creates and writes hydrotops into a 'hydrotop.gml' file from 'modell.gml' (catchments), 'pedologie.gml',
 * 'geologie.gml' and 'landuse.gml'
 * 
 * @author Dejan Antanaskovic
 */
public class HydrotopeCreationOperation implements IRunnableWithProgress
{
  private static final String COORDINATE_SYSTEM = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private final FeatureList m_landuseList;

  private final FeatureList m_pedologyList;

  private final FeatureList m_geologyList;

  private final FeatureList m_catchmentsList;

  private final IFeatureBindingCollection<IHydrotope> m_outputList;

  private final GMLWorkspace m_workspace;

  private boolean m_dissolveFeatures = true;

  private GM_MultiSurface m_workingArea = null;

  private double m_forcedSealingCorrectionFactorValue = Double.NaN;

  private boolean m_isSealingCorrectionForced = false;

  public HydrotopeCreationOperation( final FeatureList landuseList, final FeatureList pedologyList, final FeatureList geologyList, final FeatureList catchmentsList, final IFeatureBindingCollection<IHydrotope> outputList, final GMLWorkspace outputWorkspace, final GM_MultiSurface workingArea )
  {
    m_landuseList = landuseList;
    m_pedologyList = pedologyList;
    m_geologyList = geologyList;
    m_catchmentsList = catchmentsList;
    m_outputList = outputList;
    m_workspace = outputWorkspace;
    m_workingArea = workingArea;
  }

  public final void setDissolveMode( final boolean dissolveFeatures )
  {
    m_dissolveFeatures = dissolveFeatures;
  }

  /**
   * Forces the given sealing correction factor, instead the one from the landuse data. <br>
   * If the given value is Double.NaN, then the factor value is not forced.
   */
  public final void forceSealingCorrectionFactor( final double value )
  {
    m_forcedSealingCorrectionFactorValue = value;
    m_isSealingCorrectionForced = !Double.isNaN( value );
  }

  private final FeatureListGeometryIntersector getIntersector( )
  {
    final FeatureListGeometryIntersector geometryIntersector = new FeatureListGeometryIntersector();
    if( m_workingArea == null )
    {
      geometryIntersector.addFeatureList( m_pedologyList );
      geometryIntersector.addFeatureList( m_geologyList );
      geometryIntersector.addFeatureList( m_catchmentsList );
      geometryIntersector.addFeatureList( m_landuseList );
      m_outputList.clear();
    }
    else
    {
      final GM_Envelope envelope = m_workingArea.getEnvelope();
      geometryIntersector.addFeatureList( m_pedologyList.query( envelope, null ) );
      geometryIntersector.addFeatureList( m_geologyList.query( envelope, null ) );
      geometryIntersector.addFeatureList( m_catchmentsList.query( envelope, null ) );
      geometryIntersector.addFeatureList( m_landuseList.query( envelope, null ) );
    }
    return geometryIntersector;
  }

  /**
   * TODO implements/extends ICoreRunnalbeWithProgress or WorkspaceJob -> run method return status!!!!
   * 
   * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void run( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.HydrotopeCreationOperation.0" ), 100 ); //$NON-NLS-1$

    final FeatureListGeometryIntersector geometryIntersector = getIntersector();
    final List<Polygon> intersectionList;
    try
    {
      progress.setTaskName( Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.HydrotopeCreationOperation.1" ) ); //$NON-NLS-1$
      intersectionList = geometryIntersector.intersect( progress.newChild( 80 ) );

      progress.setTaskName( Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.HydrotopeCreationOperation.2" ) ); //$NON-NLS-1$
      progress.setWorkRemaining( intersectionList.size() );

      if( m_outputList.size() > 0 )
      {
        final Geometry intersectionArea = JTSAdapter.jtsFactory.createGeometryCollection( intersectionList.toArray( new Polygon[intersectionList.size()] ) ).buffer( 0.0 );
        // FIXME: check if the coordinate system is really required, it was always null in previous implementation
        final GM_Envelope gmEnvelope = JTSAdapter.wrap( intersectionArea.getEnvelopeInternal(), COORDINATE_SYSTEM );
        final List<IHydrotope> list = m_outputList.query( gmEnvelope );
        for( final IHydrotope hydrotop : list )
        {
          final Geometry g = JTSAdapter.export( hydrotop.getGeometry() );
          if( g.disjoint( intersectionArea ) || g.touches( intersectionArea ) )
            continue;

          if( g.coveredBy( intersectionArea ) )
            m_outputList.remove( hydrotop );
          else
          {
            final Geometry geometry = g.difference( intersectionArea );
            final GM_MultiSurface hydrotopGeometry = toMultiSurface( geometry, COORDINATE_SYSTEM );
            if( hydrotopGeometry != null )
              hydrotop.setGeometry( hydrotopGeometry );
            else
            {
              // TODO what to do?
            }
          }
        }
      }

      int count = 0;

      final IGMLSchema gmlSchema = m_workspace.getGMLSchema();
      final IFeatureType hydrotopeFT = gmlSchema.getFeatureType( Hydrotop.QNAME );
      final IRelationType sudsMemberRT = (IRelationType) hydrotopeFT.getProperty( Hydrotop.QNAME_PROP_SUD_MEMBERS );
      final IRelationType catchmentMemberRT = (IRelationType) hydrotopeFT.getProperty( Hydrotop.QNAME_PROP_CATCHMENT_MEMBER );

      for( final Geometry geometry : intersectionList )
      {
        if( count % 100 == 0 )
        {
          progress.subTask( Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.HydrotopeCreationOperation.3", count, intersectionList.size() ) ); //$NON-NLS-1$
          // TODO: belongs to the end of this loop, but there are just too many else's
          // Better: put into sub-method and 'return' instead of 'continue'
          ProgressUtilities.worked( monitor, 100 );
        }
        count++;

        final FeatureIntersection userData = (FeatureIntersection) geometry.getUserData();
        final Feature[] features = userData.getFeatures();
        // we always expect an intersection of subcatchments, landuse, geology and pedology
        if( features.length != 4 )
          continue;

        final IHydrotope hydrotop = m_outputList.addNew( IHydrotope.QNAME );

        for( final Feature feature : features )
        {
          if( feature instanceof org.kalypso.model.hydrology.binding.model.Catchment )
          {
            final IFeatureType featureType = feature.getFeatureType();
            final String href = String.format( "modell.gml#%s", feature.getId() ); //$NON-NLS-1$
            final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( hydrotop, catchmentMemberRT, featureType, href, null, null, null, null, null );
            hydrotop.setCatchmentMember( lnk );
          }
          else if( feature instanceof Landuse )
          {
            final Landuse landuse = (Landuse) feature;
            final Object landuseClassLink = landuse.getLanduse();
            final Feature featureLanduse = FeatureHelper.resolveLinkedFeature( m_workspace, landuseClassLink );
            if( featureLanduse == null )
              hydrotop.setLanduse( ((XLinkedFeature_Impl) landuseClassLink).getFeatureId() );
            else
              hydrotop.setLanduse( featureLanduse.getName() );
            if( m_isSealingCorrectionForced )
            {
              hydrotop.setCorrSealing( m_forcedSealingCorrectionFactorValue );
            }
            else
            {
              final Double corrSealing = landuse.getCorrSealing();
              hydrotop.setCorrSealing( corrSealing );
            }
            final IFeatureBindingCollection<Feature> landuseSudsCollection = landuse.getSudCollection();
            final IFeatureBindingCollection<ISuds> hydrotopeSudsCollection = hydrotop.getSudCollection();
            final FeatureList hydrotopeFeatureList = hydrotopeSudsCollection.getFeatureList();
            for( final Feature sudsFeature : landuseSudsCollection )
            {
              // TODO check why landuse have the collection of suds, when hydrotop may be connected to just one sud
              if( sudsFeature instanceof XLinkedFeature_Impl )
              {
                final IFeatureType ft = sudsFeature.getFeatureType();
                final String href = String.format( "suds.gml#%s", ((XLinkedFeature_Impl) sudsFeature).getFeatureId() ); //$NON-NLS-1$

                final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( hydrotop, sudsMemberRT, ft, href, null, null, null, null, null );
                hydrotopeFeatureList.add( lnk );
              }
            }
          }
          else if( feature instanceof SoilType )
          {
            final Object soiltypeClassLink = ((SoilType) feature).getSoilType();
            final String value;
            if( soiltypeClassLink instanceof XLinkedFeature_Impl )
              value = ((XLinkedFeature_Impl) soiltypeClassLink).getFeatureId();
            else
              value = soiltypeClassLink.toString().substring( soiltypeClassLink.toString().indexOf( "#" ) + 1 ); //$NON-NLS-1$
            hydrotop.setSoilType( value );
          }
          else if( feature instanceof Geology )
          {
            final Geology geology = (Geology) feature;
            hydrotop.setMaxPerkolationRate( geology.getMaxPerkulationsRate() );
            hydrotop.setGWFactor( geology.getGWFactor() );
          }
        }

        if( m_dissolveFeatures )
        {
          final GM_Envelope featureGeometryEnvelope = JTSAdapter.wrap( geometry.getEnvelopeInternal(), COORDINATE_SYSTEM );
          final List<IHydrotope> list = m_outputList.query( featureGeometryEnvelope );
          final List<Feature> featuresToMergeWith = new ArrayList<Feature>();
          final List<Geometry> geometriesToMergeWith = new ArrayList<Geometry>();
          for( final Feature f : list )
          {
            if( f == hydrotop )
              continue;

            final Geometry g = JTSAdapter.export( f.getDefaultGeometryPropertyValue() );
            if( g == null )
            {
              System.out.println( "Broken Feature:" + f + " MORE:" + f.getDefaultGeometryPropertyValue() );
              m_outputList.remove( f );
              continue;
            }
            else if( geometry.intersects( g ) && hydrotop.isEqualByPropertiesWith( f ) )
            {
              featuresToMergeWith.add( f );
              geometriesToMergeWith.add( g );
            }
          }
          if( !featuresToMergeWith.isEmpty() )
          {
            geometriesToMergeWith.add( geometry );

            Geometry union = null;

            final GeometryCollection geometryCollection = new GeometryFactory().createGeometryCollection( GeometryFactory.toGeometryArray( geometriesToMergeWith ) );
            try
            {
              union = geometryCollection.union();
              m_outputList.removeAll( featuresToMergeWith );
            }
            catch( final Exception e )
            {
              Logger.getLogger( getClass().getName() ).log( Level.WARNING, e.getLocalizedMessage() );
              try
              {
                union = geometryCollection.buffer( 0.0 );
                m_outputList.removeAll( featuresToMergeWith );
              }
              catch( final Exception e2 )
              {
                try
                {
                  union = geometry;
                  for( int i = 0; i < geometriesToMergeWith.size() - 1; i++ )
                    union = union.union( geometriesToMergeWith.get( i ) );
                  m_outputList.removeAll( featuresToMergeWith );
                }
                catch( final Exception e3 )
                {
                  e.printStackTrace();
                  continue;
                }
              }
            }
            final GM_MultiSurface hydrotopGeometry = toMultiSurface( union, COORDINATE_SYSTEM );
            hydrotop.setGeometry( hydrotopGeometry );
          }
          else
          {
            final GM_MultiSurface lGeometry = toMultiSurface( geometry, COORDINATE_SYSTEM );
            hydrotop.setGeometry( lGeometry );
          }
        }
        else
        {
          final GM_MultiSurface lGeometry = toMultiSurface( geometry, COORDINATE_SYSTEM );
          hydrotop.setGeometry( lGeometry );
        }
        hydrotop.setName( hydrotop.getId() );
      }
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
  }

  private final GM_MultiSurface toMultiSurface( final Geometry geometry, final String crs )
  {
    try
    {
      final GM_Object newGeometry = JTSAdapter.wrap( geometry, crs );
      if( newGeometry instanceof GM_MultiSurface )
        return (GM_MultiSurface) newGeometry;
      else if( newGeometry instanceof GM_Surface )
      {
        final ArrayList<GM_Surface< ? >> arrayList = new ArrayList<GM_Surface< ? >>();
        arrayList.add( (GM_Surface< ? >) newGeometry );
        final GM_MultiSurface multiSurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_MultiSurface( arrayList.toArray( new GM_Surface[0] ), newGeometry.getCoordinateSystem() );
        return multiSurface;
      }
      else if( newGeometry instanceof GM_MultiPrimitive )
      {
        final GeometryCollection lCol = (GeometryCollection) JTSAdapter.export( newGeometry );
        return toMultiSurface( lCol.buffer( 0.0 ), crs );
      }
      return null;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

// public static final boolean isPointInsidePolygon( final GM_Position[] pos, final GM_Point point )
// {
// boolean isInside = false;
// int nPoints = pos.length;
//
// if( pos[nPoints - 1].equals( pos[0] ) == true )
// nPoints -= 1;
//
// int j = 0;
// for( int i = 0; i < nPoints; i++ )
// {
// j++;
// if( j == nPoints )
// j = 0;
//
// /*
// * if (y[i] < pointY && y[j] >= pointY || y[j] < pointY && y[i] >= pointY) { nx = x - pos[0].x; if (x[i] + (pointY
// * - y[i]) / (y[j] - y[i]) * (x[j] - x[i]) < pointX) { isInside = !isInside; } }
// */
//
// if( pos[i].getY() < point.getY() && (pos[j].getY() > point.getY() || AboutEqual( pos[j].getY(), point.getY() )) ||
// (pos[j].getY() < point.getY() && (pos[i].getY() > point.getY()
// /* || AboutEqual( pos[i].getY(), point.getY()) */)) )
// {
// if( (pos[i].getX() - point.getX()) + (point.getY() - pos[i].getY()) / (pos[j].getY() - pos[i].getY()) *
// (pos[j].getX() - pos[i].getX()) < 0 )
// {
// isInside = !isInside;
// }
// else if( isOnLine( pos[i], pos[j], point.getPosition() ) )
// {
// isInside = true;
// break;
// }
// }
// }
//
// return isInside;
// }

// public static final boolean AboutEqual( final double x, final double y )
// {
// return Math.abs( x - y ) <= GM_Position.MUTE;
// }

// public static final boolean isPointInsidePolygon( final GM_Object gmObject, final GM_Point point )
// {
// if( gmObject instanceof GM_Surface )
// {
// final GM_SurfacePatch patch = ((GM_Surface< ? >) gmObject).get( 0 );
// final GM_Position[] exteriorRing = patch.getExteriorRing();
// final GM_Position[][] interiorRings = patch.getInteriorRings();
//
// if( isPointInsidePolygon( exteriorRing, point ) == false )
// return false;
//
// if( interiorRings != null )
// for( final GM_Position[] interiorRing : interiorRings )
// if( isPointInsidePolygon( interiorRing, point ) == true )
// return false;
//
// return true;
// }
//
// if( gmObject instanceof GM_MultiSurface )
// {
// final GM_Surface< ? >[] surfaces = ((GM_MultiSurface) gmObject).getAllSurfaces();
//
// if( surfaces != null )
// for( final GM_Surface< ? > surface : surfaces )
// if( isPointInsidePolygon( surface, point ) == true )
// return true;
//
// return false;
// }
//
// return false;
// }

// private static final boolean isOnLine( final GM_Position endPoint1, final GM_Position endPoint2, final GM_Position
// checkPoint )
// {
// return (checkPoint.getY() - endPoint1.getY()) / (endPoint2.getY() - endPoint1.getY()) == (checkPoint.getX() -
// endPoint1.getX()) / (endPoint2.getX() - endPoint1.getX());
// }

}
