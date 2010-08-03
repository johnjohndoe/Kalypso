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
package org.kalypso.convert.namodel.hydrotope;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.convert.namodel.FeatureListGeometryIntersector;
import org.kalypso.convert.namodel.schema.binding.Geology;
import org.kalypso.convert.namodel.schema.binding.Hydrotop;
import org.kalypso.convert.namodel.schema.binding.Hydrotop.HYDROTOP_TYPE;
import org.kalypso.convert.namodel.schema.binding.Landuse;
import org.kalypso.convert.namodel.schema.binding.SoilType;
import org.kalypso.convert.namodel.schema.binding.suds.Greenroof;
import org.kalypso.convert.namodel.schema.binding.suds.Swale;
import org.kalypso.convert.namodel.schema.binding.suds.SwaleInfiltrationDitch;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
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
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;

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

  private final FeatureList m_outputList;

  private final GMLWorkspace m_workspace;

  private final IFeatureType m_featureType;

  private boolean m_dissolveFeatures = true;

  private GM_MultiSurface m_workingArea = null;

  private double m_forcedSealingCorrectionFactorValue = Double.NaN;

  private boolean m_isSealingCorrectionForced = false;

  public HydrotopeCreationOperation( final FeatureList landuseList, final FeatureList pedologyList, final FeatureList geologyList, final FeatureList catchmentsList, final FeatureList outputList, final GMLWorkspace outputWorkspace )
  {
    m_landuseList = landuseList;
    m_pedologyList = pedologyList;
    m_geologyList = geologyList;
    m_catchmentsList = catchmentsList;
    m_outputList = outputList;
    m_workspace = outputWorkspace;
    m_featureType = m_workspace.getGMLSchema().getFeatureType( NaModelConstants.HYDRO_ELEMENT_FT );
  }

  public HydrotopeCreationOperation( final FeatureList landuseList, final FeatureList pedologyList, final FeatureList geologyList, final FeatureList catchmentsList, final FeatureList outputList, final GMLWorkspace outputWorkspace, final GM_MultiSurface workingArea )
  {
    m_landuseList = landuseList;
    m_pedologyList = pedologyList;
    m_geologyList = geologyList;
    m_catchmentsList = catchmentsList;
    m_outputList = outputList;
    m_workspace = outputWorkspace;
    m_featureType = m_workspace.getGMLSchema().getFeatureType( NaModelConstants.HYDRO_ELEMENT_FT );
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
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.0" ), 100 ); //$NON-NLS-1$

    Date lDate = new Date();

    IWorkbench wb = PlatformUI.getWorkbench();

    final FeatureListGeometryIntersector geometryIntersector = getIntersector();
    final List<MultiPolygon> intersectionList;
    try
    {
      progress.setTaskName( Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.1" ) ); //$NON-NLS-1$
      intersectionList = geometryIntersector.intersect( progress.newChild( 50 ) );

      System.out.println( "INTERSECTION LIST: " + intersectionList.size() );
      progress.setTaskName( Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.2" ) ); //$NON-NLS-1$
      progress.setWorkRemaining( intersectionList.size() );

// if (intersectionList.size() > 0) {
// m_outputList.clear();
// for( Iterator iterator = intersectionList.iterator(); iterator.hasNext(); )
// {
// Geometry multiPolygon = (Geometry) iterator.next();
// final Hydrotop hydrotop = (Hydrotop) m_workspace.createFeature( null, null, m_featureType );
// GM_MultiSurface lGeometry = (GM_MultiSurface) JTSAdapter.wrap( multiPolygon );
// if( lGeometry == null )
// System.out.println( "lGeometrijaaa null #1" );
// hydrotop.setGeometry( lGeometry );
// hydrotop.setProperty( NaModelConstants.HYDRO_PROP_AREA, multiPolygon.getArea() );
// m_outputList.add( hydrotop );
// }
// // m_outputList
// return;
//
// }
//
      if( m_outputList.size() > 0 )
      {
        final Geometry intersectionArea = new GeometryFactory().createGeometryCollection( intersectionList.toArray( new MultiPolygon[] {} ) ).buffer( 0.0 );
        // FIXME: check if the coordinate system is really required, it was always null in previous implementation
        final GM_Envelope gmEnvelope = JTSAdapter.wrap( intersectionArea.getEnvelopeInternal(), COORDINATE_SYSTEM );
        final List<Feature> list = m_outputList.query( gmEnvelope, null );
        for( final Feature feature : list )
        {
          feature.getDefaultGeometryPropertyValue();
          if( feature instanceof Hydrotop )
          {
            final Hydrotop hydrotop = (Hydrotop) feature;
            final Geometry g = JTSAdapter.export( hydrotop.getGeometry() );
            if( g.disjoint( intersectionArea ) || g.touches( intersectionArea ) )
            {
              continue;
            }
            else if( g.coveredBy( intersectionArea ) )
            {
              m_outputList.remove( feature );
            }
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
              break;
            }
          }
        }
      }
      int count = 0;

      final IRelationType sudsMemberRT = (IRelationType) m_workspace.getGMLSchema().getFeatureType( Hydrotop.QNAME ).getProperty( Hydrotop.QNAME_PROP_SUD_MEMBERS );
      final IRelationType catchmentMemberRT = (IRelationType) m_workspace.getGMLSchema().getFeatureType( Hydrotop.QNAME ).getProperty( Hydrotop.QNAME_PROP_CATCHMENT_MEMBER );

      for( final Geometry geometry : intersectionList )
      {
        if( count % 100 == 0 )
          progress.subTask( Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.3", count, intersectionList.size() ) ); //$NON-NLS-1$
        count++;

        // TODO: belongs to the end of this loop, but there are just too many else's
        // Better: put into sub-method and 'return' instead of 'continue'
        ProgressUtilities.worked( monitor, 1 );

        if( geometry.getArea() == 0.0 )
          continue;

        final Hydrotop hydrotop = (Hydrotop) m_workspace.createFeature( null, null, m_featureType );

        final Point interiorPoint = geometryIntersector.getJustInteriorPointFixed( geometry );
        if( interiorPoint == null )
        {
          System.out.println( "NULL + " + geometry );
          continue;
        }

        final GM_Envelope envelope = JTSAdapter.wrap( interiorPoint.getEnvelopeInternal(), COORDINATE_SYSTEM );
        final GM_Point point = (GM_Point) JTSAdapter.wrap( interiorPoint );

        final List<Object> catchmentList = m_catchmentsList.query( envelope, null );
        if( catchmentList.size() == 0 )
          continue;
        else
        {
          boolean catchmentFound = false;
          for( final Object object : catchmentList )
          {
            final Feature catchment = (Feature) object;

            GM_Object catchmentGeo = catchment.getDefaultGeometryPropertyValue();
            boolean l_bTest = isPointInsidePolygon( catchmentGeo, point );
// if( catchmentGeo.contains( point ) )
            if( l_bTest )
            {
              catchmentFound = true;
              final IFeatureType featureType = catchment.getFeatureType();
              final String href = String.format( "modell.gml#%s", catchment.getId() ); //$NON-NLS-1$
              final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( hydrotop, catchmentMemberRT, featureType, href, null, null, null, null, null );
              hydrotop.setCatchmentMember( lnk );
              break;
            }
          }
          if( !catchmentFound )
            continue;
        }

        final List<Landuse> landuseList = m_landuseList.query( envelope, null );

        if( landuseList.size() > 0 )
        {
          Landuse landuse = null;
          for( final Landuse l : landuseList )
          {
            GM_Object normaleLanduse = l.getDefaultGeometryPropertyValue();

            boolean l_bTest = isPointInsidePolygon( normaleLanduse, point );
// if( normaleLanduse.contains( point ) )
            if( l_bTest )
            {
              landuse = l;
              break;
            }
          }
          if( landuse == null )
            continue;

          final Object landuseClassLink = landuse.getLanduse();
          final Feature featureLanduse = FeatureHelper.resolveLinkedFeature( m_workspace, landuseClassLink );
          if( featureLanduse == null )
            hydrotop.setLanduse( ((XLinkedFeature_Impl) landuseClassLink).getFeatureId() );
          else
            hydrotop.setLanduse( featureLanduse.getName() );
          hydrotop.setDrainageType( landuse.getDrainageType() );
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
          final IFeatureBindingCollection<Feature> hydrotopeSudsCollection = hydrotop.getSudCollection();
          for( final Feature feature : landuseSudsCollection )
          {
            // TODO check why landuse have the collection of suds, when hydrotop may be connected to just one sud
            if( feature instanceof XLinkedFeature_Impl )
            {
              final IFeatureType ft = feature.getFeatureType();
              final String href = String.format( "suds.gml#%s", ((XLinkedFeature_Impl) feature).getFeatureId() ); //$NON-NLS-1$

              final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( hydrotop, sudsMemberRT, ft, href, null, null, null, null, null );
              hydrotopeSudsCollection.add( lnk );

              final Feature sudFeature = ((XLinkedFeature_Impl) feature).getFeature();
              if( sudFeature instanceof Swale || sudFeature instanceof SwaleInfiltrationDitch )
                hydrotop.setHydrotopType( HYDROTOP_TYPE.MULDEN_RIGOLE );
              else if( sudFeature instanceof Greenroof )
                hydrotop.setHydrotopType( HYDROTOP_TYPE.DACHBEGRUENUNG );
              else
                hydrotop.setHydrotopType( HYDROTOP_TYPE.BODENSPEICHER );
            }
            else
              hydrotop.setHydrotopType( HYDROTOP_TYPE.BODENSPEICHER );
          }
        }
        else
          continue;

        final List<SoilType> soilTypesList = m_pedologyList.query( envelope, null );
        if( soilTypesList.size() > 0 )
        {
          SoilType soilType = null;
          for( final SoilType s : soilTypesList )
          {
            boolean l_bTest = isPointInsidePolygon( s.getDefaultGeometryPropertyValue(), point );
// if( s.getDefaultGeometryPropertyValue().contains( point ) )
            if( l_bTest )
            {
              soilType = s;
              break;
            }
          }
          if( soilType == null )
            continue;
          final Object soiltypeClassLink = soilType.getSoilType();
          final String value; //$NON-NLS-1$
          if( soiltypeClassLink instanceof XLinkedFeature_Impl )
            value = ((XLinkedFeature_Impl) soiltypeClassLink).getFeatureId();
          else
            value = soiltypeClassLink.toString().substring( soiltypeClassLink.toString().indexOf( "#" ) + 1 ); //$NON-NLS-1$
          hydrotop.setSoilType( value );
        }
        else
          continue;

        final List<Geology> geologyList = m_geologyList.query( envelope, null );
        if( geologyList.size() > 0 )
        {
          Geology geology = null;
          for( final Geology g : geologyList )
          {
            boolean l_bTest = isPointInsidePolygon( g.getDefaultGeometryPropertyValue(), point );
// if( g.getDefaultGeometryPropertyValue().contains( point ) )
            if( l_bTest )
            {
              geology = g;
              break;
            }
          }
          if( geology == null )
            continue;
          hydrotop.setMaxPerkolationRate( geology.getMaxPerkulationsRate() );
          hydrotop.setGWFactor( geology.getGWFactor() );
        }
        else
          continue;

        if( m_dissolveFeatures )
        {
          final GM_Envelope featureGeometryEnvelope = JTSAdapter.wrap( geometry.getEnvelopeInternal(), COORDINATE_SYSTEM );
          final List<Feature> list = m_outputList.query( featureGeometryEnvelope, null );
          final List<Feature> featuresToMergeWith = new ArrayList<Feature>();
          final List<Geometry> geometriesToMergeWith = new ArrayList<Geometry>();
          for( final Feature f : list )
          {
            final Geometry g = JTSAdapter.export( f.getDefaultGeometryPropertyValue() );
            if( g == null )
            {
              System.out.println( "Broken Feature:" + f + " MORE:" + f.getDefaultGeometryPropertyValue() );
              m_outputList.remove( f );
              continue;
            }
            if( geometry.intersects( g ) )
              if( hydrotop.isEqualByPropertiesWith( f ) )
              {
                featuresToMergeWith.add( f );
                geometriesToMergeWith.add( g );
              }
          }
          if( !featuresToMergeWith.isEmpty() )
          {
            geometriesToMergeWith.add( geometry );

            Geometry union = null;

            GeometryCollection geometryCollection = new GeometryFactory().createGeometryCollection( GeometryFactory.toGeometryArray( geometriesToMergeWith ) );
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
              catch( Exception e2 )
              {
                System.out.println( "Karamba, buff0r ne rabotaet ..." );
                try
                {
                  union = geometry;
                  for( int i = 0; i < geometriesToMergeWith.size() - 1; i++ )
                    union = union.union( geometriesToMergeWith.get( i ) );
                  m_outputList.removeAll( featuresToMergeWith );
                }
                catch( Exception e3 )
                {
                  System.out.println( "NU VSE. ETO PI...." );
                  continue;
                }
              }
            }
            final GM_MultiSurface hydrotopGeometry = toMultiSurface( union, COORDINATE_SYSTEM );
            if( hydrotopGeometry != null )
              hydrotop.setGeometry( hydrotopGeometry );
            else
            {
              System.out.println( "A sho delat-ta" );
            }
            hydrotop.setProperty( NaModelConstants.HYDRO_PROP_AREA, union.getArea() );
          }
          else
          {
            GM_MultiSurface lGeometry = (GM_MultiSurface) JTSAdapter.wrap( geometry );
            if( lGeometry == null )
              System.out.println( "lGeometrijaaa null #1" );
            hydrotop.setGeometry( lGeometry );
            hydrotop.setProperty( NaModelConstants.HYDRO_PROP_AREA, geometry.getArea() );
          }
        }
        else
        {
          GM_MultiSurface lGeometry = (GM_MultiSurface) JTSAdapter.wrap( geometry );
          if( lGeometry == null )
            System.out.println( "lGeometrijaaa null #2" );
          hydrotop.setGeometry( lGeometry );
          hydrotop.setProperty( NaModelConstants.HYDRO_PROP_AREA, geometry.getArea() );
        }
        hydrotop.setName( hydrotop.getId() );
        m_outputList.add( hydrotop );
      }
    }
// catch( final GM_Exception e )
// {
// throw new InvocationTargetException( e );
// }
// catch( final CoreException e )
// {
// throw new InvocationTargetException( e );
// }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }

    System.out.println( "OUTPUT LIST: " + m_outputList.size() );

    System.out.println( "Result: " + ((new Date()).getTime() - lDate.getTime()) );

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
// final List<GM_Surface< ? >> arrayList = new ArrayList<GM_Surface< ? >>();
// arrayList.add( (GM_Surface< ? >) newGeometry );
        final ArrayList<GM_Surface> arrayList = new ArrayList<GM_Surface>();
        arrayList.add( (GM_Surface) newGeometry );
        final GM_MultiSurface multiSurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_MultiSurface( arrayList.toArray( new GM_Surface[0] ), newGeometry.getCoordinateSystem() );
        return multiSurface;
      }
      else if( newGeometry instanceof GM_MultiPrimitive )
      {
        GeometryCollection lCol = (GeometryCollection) JTSAdapter.export( newGeometry );
        return toMultiSurface( lCol.buffer( 0.0 ), crs );
// final ArrayList<GM_Surface> arrayList = new ArrayList<GM_Surface>();
// for( final GM_Object lGM_Obj: ((GM_MultiPrimitive)newGeometry).getAll() ){
// arrayList.add( (GM_Surface) lGM_Obj );
// }
// final GM_MultiSurface multiSurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_MultiSurface(
// arrayList.toArray( new GM_Surface[0] ), newGeometry.getCoordinateSystem() );
// return multiSurface;
      }
      System.out.println( "A kak takoja sluchilas ta" );
      System.out.println( "A vot kak:" + newGeometry );
      return null;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      System.out.println( "A sho sluchilas ta" );
      return null;
    }
  }

  public static final boolean isPointInsidePolygon( final GM_Position[] pos, final GM_Point point )
  {
    boolean isInside = false;
    int nPoints = pos.length;

    if( pos[nPoints - 1].equals( pos[0] ) == true )
      nPoints -= 1;

    int j = 0;
    for( int i = 0; i < nPoints; i++ )
    {
      j++;
      if( j == nPoints )
        j = 0;

      /*
       * if (y[i] < pointY && y[j] >= pointY || y[j] < pointY && y[i] >= pointY) { nx = x - pos[0].x; if (x[i] + (pointY
       * - y[i]) / (y[j] - y[i]) * (x[j] - x[i]) < pointX) { isInside = !isInside; } }
       */

      if( pos[i].getY() < point.getY() && (pos[j].getY() > point.getY() || AboutEqual( pos[j].getY(), point.getY() )) || (pos[j].getY() < point.getY() && (pos[i].getY() > point.getY()
      /* || AboutEqual( pos[i].getY(), point.getY()) */)) )
      {
        if( (pos[i].getX() - point.getX()) + (point.getY() - pos[i].getY()) / (pos[j].getY() - pos[i].getY()) * (pos[j].getX() - pos[i].getX()) < 0 )
        {
          isInside = !isInside;
        }
        else if( isOnLine( pos[i], pos[j], point.getPosition() ) )
        {
          isInside = true;
          break;
        }
      }
    }

    return isInside;
  }

  public static final boolean AboutEqual( final double x, final double y )
  {
    return Math.abs( x - y ) <= GM_Position.MUTE;
  }

  public static final boolean isPointInsidePolygon( final GM_Object gmObject, final GM_Point point )
  {
    if( gmObject instanceof GM_Surface )
    {
      final GM_SurfacePatch patch = ((GM_Surface< ? >) gmObject).get( 0 );
      final GM_Position[] exteriorRing = patch.getExteriorRing();
      final GM_Position[][] interiorRings = patch.getInteriorRings();

      if( isPointInsidePolygon( exteriorRing, point ) == false )
        return false;

      if( interiorRings != null )
        for( int i = 0; i < interiorRings.length; i++ )
          if( isPointInsidePolygon( interiorRings[i], point ) == true )
            return false;

      return true;
    }

    if( gmObject instanceof GM_MultiSurface )
    {
      final GM_Surface< ? >[] surfaces = ((GM_MultiSurface) gmObject).getAllSurfaces();

      if( surfaces != null )
        for( int i = 0; i < surfaces.length; i++ )
          if( isPointInsidePolygon( surfaces[i], point ) == true )
            return true;

      return false;
    }

    return false;
  }

  private static final boolean isOnLine( final GM_Position endPoint1, final GM_Position endPoint2, final GM_Position checkPoint )
  {
    return (checkPoint.getY() - endPoint1.getY()) / (endPoint2.getY() - endPoint1.getY()) == (checkPoint.getX() - endPoint1.getX()) / (endPoint2.getX() - endPoint1.getX());
  }

}
