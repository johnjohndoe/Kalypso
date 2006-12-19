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
package org.kalypso.model.wspm.ui.wizard;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.schema.gml.ProfileCacherFeaturePropertyFunction;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.wizard.FeatureThemeWizardUtilitites.FOUND_PROFILES;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * A wizard to create profile deviders from lines/polygones.
 * 
 * @author Gernot Belger
 */
public class CreateProfileDeviderWizard extends Wizard
{
  private ArrayChooserPage m_profileChooserPage;

  private CreateProfileDeviderPage m_deviderPage;

  private final FOUND_PROFILES m_foundProfiles;

  public CreateProfileDeviderWizard( final FOUND_PROFILES foundProfiles )
  {
    m_foundProfiles = foundProfiles;

    setWindowTitle( "Flieﬂzonen erzeugen" );
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    m_profileChooserPage = new ArrayChooserPage( m_foundProfiles.foundProfiles, new Object[] {}, m_foundProfiles.selectedProfiles, "profileFeaturesChooserPage", "Profile ausw‰hlen", null );
    m_profileChooserPage.setLabelProvider( new GMLEditorLabelProvider2() );
    m_profileChooserPage.setMessage( "Bitte w‰hlen Sie aus, in welchen Profilen Flieﬂzonen erzeugt werden sollen." );

    m_deviderPage = new CreateProfileDeviderPage( m_foundProfiles.theme );

    addPage( m_profileChooserPage );
    addPage( m_deviderPage );

    super.addPages();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#dispose()
   */
  @Override
  public void dispose( )
  {
    m_profileChooserPage.getLabelProvider().dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] choosen = m_profileChooserPage.getChoosen();
    if( choosen.length == 0 )
      return true;

    final FeatureList lineFeatures = m_deviderPage.getFeatures();
    final IPropertyType lineGeomProperty = m_deviderPage.getGeomProperty();
    final String deviderType = m_deviderPage.getDeviderType();

    final IKalypsoFeatureTheme commandTarget = m_foundProfiles.theme;

    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( "Flieﬂzonen erzeugen", 1 + choosen.length );

        try
        {
          final FeatureChange[] changes = createDevider( choosen, lineFeatures, lineGeomProperty, deviderType, monitor );
          if( changes.length > 0 )
          {
            final GMLWorkspace gmlworkspace = changes[0].getFeature().getWorkspace();
            final ICommand command = new ChangeFeaturesCommand( gmlworkspace, changes );
            commandTarget.postCommand( command, null );
          }
        }
        catch( final Exception e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }

        return Status.OK_STATUS;
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), false, true, runnable );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Fehler beim Erzeugen der Flieﬂzonen", status );

    return status.isOK();
  }

  @SuppressWarnings("unchecked")
  protected static FeatureChange[] createDevider( final Object[] profileFeatures, final FeatureList lineFeatures, final IPropertyType lineGeomProperty, final String deviderType, final IProgressMonitor monitor )
  {
    monitor.beginTask( "erzeuge Begrenzer", profileFeatures.length );

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    for( final Object object : profileFeatures )
    {
      try
      {
        final Feature profileFeature = (Feature) object;
        final WspmProfile profile = new WspmProfile( profileFeature );

        final GM_Curve curve = profile.getLine();
        if( curve == null )
          continue;

        final LineString profileLine = (LineString) JTSAdapter.export( curve );
        final IProfil profil = profile.getProfil();

        // find intersectors with curve
        final GM_Envelope curveEnvelope = curve.getEnvelope();
        final List lineIntersectors = lineFeatures.query( curveEnvelope, null );
        final List<Point> pointList = new ArrayList<Point>();
        for( final Object lineF : lineIntersectors )
        {
          final Feature lineFeature = (Feature) lineF;
          final GM_Object lineGeom = (GM_Object) lineFeature.getProperty( lineGeomProperty );
          if( lineGeom == null )
            continue;

          final Geometry lineGeometry = JTSAdapter.export( lineGeom );
          final Geometry line;
          if( lineGeometry instanceof Polygon || lineGeometry instanceof MultiPolygon )
            line = lineGeometry.getBoundary();
          else
            line = lineGeometry;

          // find intersecting points
          final Geometry intersection = profileLine.intersection( line );
          final Point[] points = getPointFromGeometry( intersection );
          Collections.addAll( pointList, points );
        }

        // create marker for each point
        if( createNewDevider( profil, pointList.toArray( new Point[pointList.size()] ), deviderType ) )
          Collections.addAll( changes, ProfileFeatureFactory.toFeatureAsChanges( profil, profileFeature ) );
      }
      catch( final GM_Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
      }
      finally
      {
        monitor.worked( 1 );
      }
    }

    return changes.toArray( new FeatureChange[changes.size()] );
  }

  private static Point[] getPointFromGeometry( final Geometry points )
  {
    if( points instanceof Point )
      return new Point[] { (Point) points };

    if( points instanceof MultiPoint )
    {
      final MultiPoint mp = (MultiPoint) points;
      final Point[] result = new Point[mp.getNumGeometries()];
      for( int i = 0; i < result.length; i++ )
        result[i] = (Point) mp.getGeometryN( i );

      return result;
    }

    return new Point[] {};
  }

  /**
   * At the moment, only existing points are taken
   */
  private static boolean createNewDevider( final IProfil profil, final Point[] intersectionPoints, final String deviderType )
  {
    // find corresponding profile points // create new profile points

    final Map<Point, IProfilPoint> pointMap = new HashMap<Point, IProfilPoint>( intersectionPoints.length );

    final LinkedList<IProfilPoint> points = profil.getPoints();
    for( final IProfilPoint profilPoint : points )
    {
      try
      {
        final GM_Point pointGeom = ProfileCacherFeaturePropertyFunction.convertPoint( profilPoint );
        final Point geoPoint = (Point) JTSAdapter.export( pointGeom );

        for( final Point p : intersectionPoints )
        {
          final double newDist = geoPoint.distance( p );
          final IProfilPoint lastProfilPoint = pointMap.get( p );
          final double lastDist;
          if( lastProfilPoint != null )
          {
            final GM_Point lastPointGeom = ProfileCacherFeaturePropertyFunction.convertPoint( lastProfilPoint );
            final Point lastGeoPoint = (Point) JTSAdapter.export( lastPointGeom );
            lastDist = geoPoint.distance( lastGeoPoint );
          }
          else
            lastDist = Double.POSITIVE_INFINITY;

          if( newDist < lastDist )
            pointMap.put( p, profilPoint );
        }
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
      }
    }

    // TODO
    final IProfilDevider[] existingDeviders = profil.getDevider( deviderType );
    for( final IProfilDevider devider : existingDeviders )
      profil.removeDevider( devider );
    // TODO

    for( final IProfilPoint deviderPoint : pointMap.values() )
      profil.addDevider( deviderPoint, deviderType );

    return pointMap.size() > 0;
  }
}