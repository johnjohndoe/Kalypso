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
package org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.impl.points.ProfilPoint;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * A wizard to import profile data (right now jus as trippel) into a 1D2D Terrain Model.
 * 
 * @author Thomas Jung
 */
public class ImportTrippelWizard extends Wizard implements IWizard
{
  private static final DateFormat DF = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  private final List<Feature> m_terrainModelAdds = new ArrayList<Feature>();

  private final IRiverProfileNetworkCollection m_networkModel;

  private List<IProfil> m_profiles;

  private ImportProfilePage m_ProfilePage;

  private IRiverProfileNetwork m_network;

  public ImportTrippelWizard( final IRiverProfileNetworkCollection networkModel )
  {

    m_networkModel = networkModel;

    setWindowTitle( Messages.getString("ImportTrippelWizard.0") ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    /* Choose profile data */
    m_ProfilePage = new ImportProfilePage( "chooseProfileData", Messages.getString("ImportTrippelWizard.2"), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_ProfilePage.setDescription( Messages.getString("ImportTrippelWizard.3") ); //$NON-NLS-1$

    addPage( m_ProfilePage );
  }

  /**
   * imports the profile trippel data and converts it into IProfils
   */
  protected IStatus importTrippelData( )
  {
    /* get file name from wizard */
    final File trippelFile = m_ProfilePage.getFile();

    if( trippelFile == null )
      return StatusUtilities.createWarningStatus( Messages.getString("ImportTrippelWizard.4") ); //$NON-NLS-1$

    /* read profiles, show warnings */
    final List<IProfil> profiles = new ArrayList<IProfil>();
    final List<IProfilPoint> profilPointList = new ArrayList<IProfilPoint>();

    /* file loading */
    BufferedReader fileReader = null;
    try
    {
      fileReader = new BufferedReader( new InputStreamReader( new FileInputStream( trippelFile ) ) );

      String line = null;
      final String separator = ";"; //$NON-NLS-1$
      StringTokenizer tokenizer;

      /* parameter */
      double station = 0;

      double currentStation = Double.NaN;
      int numStations = 0;

      /* File Header */
      fileReader.readLine();

      while( (line = fileReader.readLine()) != null )
      {
        /* trippel-format should be: station, x, y, z */
        tokenizer = new StringTokenizer( line, separator );

        /* continue just if there are enough values in the trippel file */
        if( tokenizer.countTokens() == 4 )
        {

          /* first value = profile station */
          station = Double.parseDouble( tokenizer.nextToken() );

          if( station != currentStation )
          {
            if( !Double.isNaN( currentStation ) )
            {
              // store current profile points as IProfil
              storeProfile( profilPointList, currentStation, profiles );
              profilPointList.clear();
              numStations = numStations + 1;
            }

            // update profile station
            currentStation = station;
          }

          IProfilPoint point = createProfilePoint( profilPointList, tokenizer );
          if( point != null )
            profilPointList.add( point );

        }
        else
        {
          // inform the user that his profile has not enough values...
          String message = Messages.getString("ImportTrippelWizard.6") + (numStations + 1) + Messages.getString("ImportTrippelWizard.7"); //$NON-NLS-1$ //$NON-NLS-2$
          return StatusUtilities.createWarningStatus( message );
        }
      }

      fileReader.close();

      /* store the last profile */
      storeProfile( profilPointList, station, profiles );
      profilPointList.clear();
      numStations = numStations + 1;

      m_profiles = profiles;

    }
    catch( FileNotFoundException e )
    {
      e.printStackTrace();
      return StatusUtilities.createWarningStatus( Messages.getString("ImportTrippelWizard.8") ); //$NON-NLS-1$
    }
    catch( IOException e )
    {
      e.printStackTrace();
      return StatusUtilities.createWarningStatus( Messages.getString("ImportTrippelWizard.9") ); //$NON-NLS-1$
    }

    finally
    {
      IOUtils.closeQuietly( fileReader );
    }

    if( profiles.size() == 0 )
    {
      return StatusUtilities.createWarningStatus( Messages.getString("ImportTrippelWizard.10") ); //$NON-NLS-1$
    }

    return Status.OK_STATUS;

  }

  /**
   * stores all gathered profile points in a new profile and adds it to the profile list
   * 
   * @param profilPointList
   *          the list of the profile points of one profile
   * @param station
   *          the current profile station
   * @param profiles
   *          the list of the allready imported profiles
   */
  private void storeProfile( List<IProfilPoint> profilPointList, double station, List<IProfil> profiles )
  {
    final String profiletype = "org.kalypso.model.wspm.tuhh.profiletype"; //$NON-NLS-1$
    final IProfil profile = ProfilFactory.createProfil( profiletype );

    profile.addPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    profile.addPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    profile.addPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    profile.addPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    for( IProfilPoint point : profilPointList )
    {
      profile.addPoint( point );
    }

    profile.setStation( station );
    profile.setName( Messages.getString("ImportTrippelWizard.12") ); //$NON-NLS-1$

    profiles.add( profile );

  }

  /**
   * creates a new profile point and adds it to the point list of the current profile
   * 
   * @param profilPointList
   *          point list of the current profile
   * @param tokenizer
   *          holds the point data (x, y, z)
   */
  private IProfilPoint createProfilePoint( final List<IProfilPoint> profilPointList, StringTokenizer tokenizer )
  {
    double x;
    double y;
    double z;

    final ProfilPoint profilPoint = new ProfilPoint();
    profilPoint.addProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    profilPoint.addProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    profilPoint.addProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    profilPoint.addProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    if( tokenizer.hasMoreElements() )
    {
      x = Double.parseDouble( tokenizer.nextToken() );
      profilPoint.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, x );
    }
    else
      return null;

    if( tokenizer.hasMoreElements() )
    {
      y = Double.parseDouble( tokenizer.nextToken() );
      profilPoint.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, y );
    }
    else
      return null;

    if( tokenizer.hasMoreElements() )
    {
      z = Double.parseDouble( tokenizer.nextToken() );
      profilPoint.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, z );
    }
    else
      return null;

    final double width;
    /* calculate width coordinate */
    if( profilPointList.size() > 0 )
    {
      width = calculateSegmentLength( profilPointList, profilPoint );
    }
    else
      width = 0;

    profilPoint.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width );

    return profilPoint;

  }

  /**
   * calculates the width coordinate by the segment length (2-dim distance of the profile points)
   * 
   * @param profilPointList
   *          point list of the current profile
   */
  private double calculateSegmentLength( List<IProfilPoint> profilPointList, ProfilPoint profilPoint )
  {
    double distance = 0;
    GeometryFactory factory = new GeometryFactory();

    /* get the segment length of the allready imported profile points */
    distance = profilPointList.get( profilPointList.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );

    /* add the segment length of the segment defined by the last imported profile point and the new to add profile point */
    double x1 = profilPointList.get( profilPointList.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    double y1 = profilPointList.get( profilPointList.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    double x2 = profilPoint.getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    double y2 = profilPoint.getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

    Coordinate coordinates1 = new Coordinate( x1, y1 );
    Coordinate coordinates2 = new Coordinate( x2, y2 );

    Point point1 = factory.createPoint( coordinates1 );
    Point point2 = factory.createPoint( coordinates2 );

    distance = distance + point1.distance( point2 );

    return distance;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    /* Collect page data */
    final IRiverProfileNetworkCollection profNetworkColl = m_networkModel;
    final List<Feature> terrainModelAdds = m_terrainModelAdds;

    /* Do import */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( Messages.getString("ImportTrippelWizard.13"), 2 ); //$NON-NLS-1$

        try
        {
          /* Import Trippel Data */
          monitor.subTask( Messages.getString("ImportTrippelWizard.14") ); //$NON-NLS-1$
          try
          {
            final IStatus status = importTrippelData();
            if( status != Status.OK_STATUS )
            {
              return status;
            }
          }
          catch( final Exception e )
          {
            return StatusUtilities.statusFromThrowable( e, Messages.getString("ImportTrippelWizard.15") ); //$NON-NLS-1$
          }

          monitor.worked( 1 );

          /* Convert Trippel Data */
          monitor.subTask( Messages.getString("ImportTrippelWizard.16") ); //$NON-NLS-1$

          final IStatus status = doImportNetwork( profNetworkColl, terrainModelAdds );

          monitor.worked( 1 );

          return status;
        }
        catch( final Throwable t )
        {
          // TODO: remove all added features from terrainModel

          throw new InvocationTargetException( t );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("ImportTrippelWizard.17"), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }

  /**
   * Converts the profiles in GML (-> terrain model).
   * 
   * @param networkCollection
   *          the GML river network, in which the profiles will be stored
   * @param addedFeatures
   */
  @SuppressWarnings("deprecation") //$NON-NLS-1$
  protected IStatus doImportNetwork( final IRiverProfileNetworkCollection networkCollection, final List<Feature> addedFeatures ) throws Exception
  {
    final IRiverProfileNetwork network = networkCollection.addNew( IRiverProfileNetwork.QNAME );
    final Feature networkFeature = network.getWrappedFeature();
    addedFeatures.add( networkFeature );

    /* Set user friendly name and descrption */
    final String desc = String.format( Messages.getString("ImportTrippelWizard.19"), m_ProfilePage.getFileName(), DF.format( new Date() ), m_ProfilePage.getFilePath() ); //$NON-NLS-1$
    network.setName( FileUtilities.nameWithoutExtension( m_ProfilePage.getFileName() ) );
    network.setDescription( desc );
    CS_CoordinateSystem crs = m_ProfilePage.getCoordinateSystem();

    GMLWorkspace workspace = networkFeature.getWorkspace();

    CS_CoordinateSystem coordinatesSystem = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    CS_CoordinateSystem targetCRS = coordinatesSystem;
    if( crs != coordinatesSystem )
    {
      targetCRS = crs;
    }
    workspace.accept( new TransformVisitor( coordinatesSystem ), networkFeature, FeatureVisitor.DEPTH_INFINITE );

    for( IProfil profile : m_profiles )
    {
      
      profile.getPointProperty( desc );
      final Feature profileFeature = FeatureHelper.addFeature( network.getWrappedFeature(), IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE, new QName( IWspmConstants.NS_WSPMPROF, Messages.getString("ImportTrippelWizard.20") ) ); //$NON-NLS-1$
      ProfileFeatureFactory.toFeature( profile, profileFeature );
      //profileFeature.getDefaultGeometryProperty().getCoordinateDimension();
      GM_Curve property = (GM_Curve) profileFeature.getProperty( WspmProfile.QNAME_LINE );
//      profileFeature.setProperty( propertyType, property )
      property.setCoordinateSystem( targetCRS );
      profileFeature.setProperty(  WspmProfile.QNAME_LINE , property );
      addedFeatures.add( profileFeature );
//      property.setCoordinateSystem( targetCRS );
      // profileFeature.setProperty( WspmProfile.QNAME_LINE, newProperty );

    }

    final GMLWorkspace workspace2 = network.getWrappedFeature().getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace2, network.getWrappedFeature(), networkFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    m_network = network;

    return Status.OK_STATUS;
  }

  public Feature[] getTerrainModelAdds( )
  {
    return m_terrainModelAdds.toArray( new Feature[m_terrainModelAdds.size()] );
  }

  public IRiverProfileNetwork getAddedRiverNetwork( )
  {
    return m_network;

  }

}
