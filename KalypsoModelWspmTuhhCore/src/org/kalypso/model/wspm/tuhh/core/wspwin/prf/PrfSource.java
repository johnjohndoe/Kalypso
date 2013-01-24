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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.StringTokenizer;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.commons.math.Range;
import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.ProfilDevider;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_GERINNE_ART;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_KENNUNG;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.wspwin.core.prf.IWspWinConstants;
import org.kalypso.wspwin.core.prf.PrfReader;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author kimwerner
 */
public class PrfSource implements IProfilSource
{
  private void readSource( final PrfReader pr, final IProfil p )
  {
    if( p == null )
      return;
    try
    {
      p.setProperty( "prfFileFormat_MetaData", pr.getMetaData() ); //$NON-NLS-1$
      p.setProperty( IWspmTuhhConstants.PROFIL_PROPERTY_STATUS, pr.getKeyValue( 3 )[1] );
      p.setProperty( IWspmTuhhConstants.PROFIL_PROPERTY_VERZWEIGUNGSKENNUNG, pr.getKeyValue( 5 )[1] );
      p.setProperty( IWspmTuhhConstants.PROFIL_PROPERTY_WASSERSPIEGEL, pr.getKeyValue( 6 )[1] );
      p.setProperty( IWspmTuhhConstants.PROFIL_PROPERTY_MEHRFELDBRUECKE, pr.getKeyValue( 7 )[1] );
      final String stat = pr.getKeyValue( 9 )[0];
      if( stat.startsWith( "STATION " ) ) //$NON-NLS-1$
        p.setStation( new Double( stat.substring( 10 ) ) );
      if( readPoints( p, pr ) > 0 )
      {
        readTrennFl( p, pr );
        readDurchStr( p, pr );
        readBordVoll( p, pr );
        readRauhheit( p, pr );
        readBewuchs( p, pr );
        readComment( p, pr );
        readGeoCoord( p, pr );
        readSinousitaet( p, pr );
        if( !(readWehr( p, pr ) || readBridge( p, pr )) )
          readBuilding( p, pr );
      }

      final String srsName = findCoordinateSystem( p );
      p.setProperty( IWspmConstants.PROFIL_PROPERTY_CRS, srsName );
    }
    catch( final IllegalArgumentException e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, e.getMessage(), e ) );
    }
  }

  // FIXME: we probably should let the user set the coordinate system
  // For the moment, we guess the coordinate system by assuming it is gauss-krueger
  private String findCoordinateSystem( final IProfil profile )
  {
    final int rwIndex = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    if( rwIndex == -1 )
      return KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final IRecord[] points = profile.getPoints();
    for( final IRecord point : points )
    {
      final Object value = point.getValue( rwIndex );
      if( value instanceof Number )
        return TimeseriesUtils.getCoordinateSystemNameForGkr( value.toString() );
    }

    return null;
  }

  private void readComment( final IProfil p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "KOM" ); //$NON-NLS-1$
    if( db == null )
      return;
    final StringBuffer sb = new StringBuffer();

    final String[] text = db.getText();
    for( int i = 0; i < text.length; i++ )
    {
      final String line = text[i];
      sb.append( line.substring( 3 ) );
      if( i != text.length - 1 )
        sb.append( System.getProperty( "line.separator" ) ); //$NON-NLS-1$
    }
    p.setComment( sb.toString() );
  }

  private void readBewuchs( final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbx = pr.getDataBlock( "AX" ); //$NON-NLS-1$
    final IDataBlock dby = pr.getDataBlock( "AY" ); //$NON-NLS-1$
    final IDataBlock dbp = pr.getDataBlock( "DP" ); //$NON-NLS-1$
    if( dbx == null || dby == null || dbp == null )
      return;

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );

    final IComponent prAx = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent prAy = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent prDp = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP );

    writePointProperty( p, prAx, dbx );
    writePointProperty( p, prAy, dby );
    writePointProperty( p, prDp, dbp );
  }

  private void writePointProperty( final IProfil p, final IComponent property, final IDataBlock db )
  {
    p.addPointProperty( property );
    final int index = p.indexOfProperty( property );
    if( index < 0 )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.7" ) + property.getName(), null ) ); //$NON-NLS-1$
      return;
    }

    final Double[] xs = db.getX();
    final Double[] ys = db.getY();
    for( int i = 0; i < xs.length; i++ )
    {
      final IRecord point = ProfilUtil.findPoint( p, i, xs[i], 0 );
      if( point != null )
        point.setValue( index, ys[i] );
    }
  }

  private void readGeoCoord( final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbh = pr.getDataBlock( "HOC" ); //$NON-NLS-1$
    final IDataBlock dbr = pr.getDataBlock( "REC" ); //$NON-NLS-1$
    if( dbh == null || dbr == null )
      return;

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );

    final IComponent hochwert = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent rechtswert = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    writePointProperty( p, hochwert, dbh );
    writePointProperty( p, rechtswert, dbr );
  }

  private void readSinousitaet( final IProfil p, final PrfReader pr )
  {
    IDataBlock db = pr.getDataBlock( "SINUOSITAET" ); //$NON-NLS-1$
    if( db == null )
      return;
    final Double[] sin = db.getX();
    final SinuositaetProfileObject profileObject = new SinuositaetProfileObject();
    final IObservation<TupleResult> observation = profileObject.getObservation();
    final TupleResult result = observation.getResult();

    final int indexKennung = result.indexOfComponent( ISinuositaetProfileObject.PROPERTY_KENNUNG );
    final int indexSinuositaet = result.indexOfComponent( ISinuositaetProfileObject.PROPERTY_SN );
    final int indexGerinneArt = result.indexOfComponent( ISinuositaetProfileObject.PROPERTY_GERINNE_ART );
    final int indexLinearFaktor = result.indexOfComponent( ISinuositaetProfileObject.PROPERTY_LF );

    final IRecord record = result.createRecord();
    record.setValue( indexKennung, SINUOSITAET_KENNUNG.fromInteger( sin[0].intValue() ).name() );
    record.setValue( indexSinuositaet, sin[1] );
    record.setValue( indexGerinneArt, SINUOSITAET_GERINNE_ART.fromInteger( sin[2].intValue() ).name() );
    record.setValue( indexLinearFaktor, sin[3] );

    result.add( record );

    p.addProfileObjects( profileObject );
  }

  private void readBuilding( final IProfil p, final PrfReader pr )
  {
    IDataBlock db = pr.getDataBlock( "EI" ); //$NON-NLS-1$
    if( db == null )
      db = pr.getDataBlock( "TRA" ); //$NON-NLS-1$
    if( db == null )
      db = pr.getDataBlock( "KRE" ); //$NON-NLS-1$
    if( db == null )
      db = pr.getDataBlock( "MAU" ); //$NON-NLS-1$
    if( db == null )
      return;
    final DataBlockHeader dbh = db.getDataBlockHeader();
    final Double[] values = db.getX();

    double rauheit = 0.0;
    final IDataBlock dbRau = pr.getDataBlock( "RAU" ); //$NON-NLS-1$
    if( dbRau != null && dbRau.getY().length > 0 )
      rauheit = dbRau.getY()[0];
    final IProfileBuilding building;
    switch( dbh.getSpecification( 8 ) )
    {
      // important: changing property-positions will cause wrong parameter for building
      case IWspWinConstants.SPEZIALPROFIL_TRAPEZ:
      {
        building = new BuildingTrapez();
        writeBuildingProperties( building, values, new String[] { IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE,
            IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X,
            IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y } );
        break;
      }
      case IWspWinConstants.SPEZIALPROFIL_KREIS:
      {
        building = new BuildingKreis();
        writeBuildingProperties( building, values, new String[] { IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE,
            IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y } );
        break;
      }
      case IWspWinConstants.SPEZIALPROFIL_EI:
      {
        building = new BuildingEi();

        writeBuildingProperties( building, values, new String[] { IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE,
            IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y } );

        break;
      }
      case IWspWinConstants.SPEZIALPROFIL_MAUL:
      {
        building = new BuildingMaul();

        writeBuildingProperties( building, values, new String[] { IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE,
            IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y } );
        break;
      }
      default:
        return;
    }
    building.setValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT, rauheit );
    p.addProfileObjects( new IProfileObject[] { building } );
  }

  private void writeBuildingProperties( final IProfileBuilding building, final Double[] val, final String[] propertyIDs )
  {
    if( building == null || val.length != propertyIDs.length )
      return;
    for( int i = 0; i < val.length; i++ )
    {
      final IComponent component = building.getObjectProperty( propertyIDs[i] );
      final QName typeName = component.getValueTypeName();
      try
      {
        if( typeName.equals( XmlTypes.XS_DECIMAL ) )
          building.setValue( component, new BigDecimal( val[i] ) );
        else
          building.setValue( component, val[i] );
      }
      catch( final IllegalArgumentException e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, e.getMessage(), e ) );
      }
    }
  }

  private boolean readBridge( final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbo = pr.getDataBlock( "OK-B" ); //$NON-NLS-1$
    final IDataBlock dbu = pr.getDataBlock( "UK-B" ); //$NON-NLS-1$
    if( dbo == null || dbu == null )
      return false;

    final IProfileBuilding bridge = new BuildingBruecke( p );
    final StringTokenizer sT = new StringTokenizer( dbu.getSecondLine(), " " ); //$NON-NLS-1$
    if( sT.countTokens() > 4 )
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.WARNING, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.20" ), null ) ); //$NON-NLS-1$

    final Double[] values = new Double[] { Double.parseDouble( sT.nextToken() ), Double.parseDouble( sT.nextToken() ), Double.parseDouble( sT.nextToken() ), Double.parseDouble( sT.nextToken() ) };
    final String[] ids = new String[] { IWspmTuhhConstants.BUILDING_PROPERTY_UNTERWASSER, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT,
        IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT };
    writeBuildingProperties( bridge, values, ids );

    p.addProfileObjects( new IProfileObject[] { bridge } );
    final IComponent okb = p.getPointPropertyFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    if( !p.hasPointProperty( okb ) )
      p.addPointProperty( okb );
    final double delta = okb == null ? 0.0001 : okb.getPrecision();

    final PolyLine polyLineO = new PolyLine( dbo.getX(), dbo.getY(), delta );
    final PolyLine polyLineU = new PolyLine( dbu.getX(), dbu.getY(), delta );
    final Range rangeO = new Range( polyLineO.getFirstX(), polyLineO.getLastX(), delta );
    final Range rangeU = new Range( polyLineU.getFirstX(), polyLineU.getLastX(), delta );
    final IComponent ukb = p.getPointPropertyFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    if( !p.hasPointProperty( ukb ) )
      p.addPointProperty( ukb );
    final int iOKB = p.indexOfProperty( okb );
    final int iUKB = p.indexOfProperty( ukb );
    for( final IRecord point : p.getPoints() )
    {
      final Double breite = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point );
      final Double hoehe = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, point );

      if( rangeO.contains( breite ) )
        point.setValue( iOKB, polyLineO.getYFor( breite ) );
      else
        point.setValue( iOKB, hoehe );
      if( rangeU.contains( breite ) )
        point.setValue( iUKB, polyLineU.getYFor( breite ) );
      else
        point.setValue( iUKB, hoehe );
    }
    return true;

  }

  private int readPoints( final IProfil p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "Gelae" ); //$NON-NLS-1$
    if( db == null )
      return 0;
    final Double[] xs = db.getX();
    final Double[] ys = db.getY();
    final IComponent cBreite = p.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent cHoehe = p.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOEHE );
    p.addPointProperty( cBreite );
    p.addPointProperty( cHoehe );
    final int iBreite = p.indexOfProperty( cBreite );
    final int iHoehe = p.indexOfProperty( cHoehe );
    for( int i = 0; i < xs.length; i++ )
    {
      final IRecord point = p.createProfilPoint();
      point.setValue( iBreite, xs[i] );
      point.setValue( iHoehe, ys[i] );
      p.addPoint( point );
    }
    return xs.length;
  }

  private void readRauhheit( final IProfil p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "RAU" ); //$NON-NLS-1$
    if( db == null )
      return;

    final String rks = db.getSecondLine().toUpperCase();

    IComponent rTyp = null;
    if( rks.startsWith( "KST" ) ) //$NON-NLS-1$
      rTyp = p.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
    else if( rks.startsWith( "KS" ) || rks.startsWith( "K-S " ) ) //$NON-NLS-1$ //$NON-NLS-2$
      rTyp = p.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );

    if( rTyp == null )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.26", rks ), null ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }
    p.addPointProperty( rTyp );
    final int index = p.indexOfProperty( rTyp );
    for( int i = 0; i < db.getCoordCount(); i++ )
    {
      final IRecord point = ProfilUtil.findPoint( p, i, db.getX()[i], 0 );
      if( point != null )
        point.setValue( index, db.getY()[i] );
    }
  }

  private void readTrennFl( final IProfil p, final PrfReader pr )
  {
    final IRecord[] points = p.getPoints();
    if( points.length == 0 )
      return;
    final IDataBlock db = pr.getDataBlock( "TRENNFLAECHEN" ); //$NON-NLS-1$
    if( db == null )
      return;
    final int pCount = db.getCoordCount();

    IRecord p1 = null;
    IRecord p2 = null;
    int pos1 = 0;
    int pos2 = 0;

    if( pCount > 0 )
    {
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
      pos1 = (int) db.getY()[0].doubleValue();
    }
    if( pCount > 1 )
    {
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
      pos2 = (int) db.getY()[1].doubleValue();
    }
    if( pCount > 2 )
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.29", p.getStation() ) //$NON-NLS-1$
      , null ) ); //$NON-NLS-1$

    if( p1 != null )
    {
      final IProfilPointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, p1 );
      marker.setInterpretedValue( (pos1 == 3) );
    }

    if( p2 != null )
    {
      final IProfilPointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, p2 );
      marker.setInterpretedValue( (pos2 == 4) );
    }
  }

  private void readDurchStr( final IProfil p, final PrfReader pr )
  {
    final IRecord[] points = p.getPoints();
    if( points.length == 0 )
      return;
    final IDataBlock db = pr.getDataBlock( "DURCHSTROEMTE" ); //$NON-NLS-1$
    if( db == null )
      return;
    final int pCount = db.getCoordCount();

    IRecord p1 = null;
    IRecord p2 = null;

    if( pCount > 0 )
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
    if( pCount > 1 )
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
    if( pCount > 2 )
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.32", p.getStation() ) //$NON-NLS-1$
      , null ) ); //$NON-NLS-1$

    if( p1 != null )
    {
      final IProfilPointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, p1 );
      marker.setValue( true );
    }

    if( p2 != null )
    {
      final IProfilPointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, p2 );
      marker.setValue( true );
    }
  }

  private void readWehrtrenner( final double[] values, final IProfil p, final PrfReader pr )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );

    final IDataBlock dbt = pr.getDataBlock( "TRENNLINIE" ); //$NON-NLS-1$
    if( dbt == null )
      return;
    final Double[] pos = dbt.getX();
    for( int i = 0; i < pos.length; i++ )
    {
      final IRecord point = ProfilUtil.findPoint( p, pos[i], 0 );
      if( point != null )
        if( values != null && values.length > i + 1 )
        {
          final IComponent cWehr = provider.getPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR );

          final ProfilDevider devider = new ProfilDevider( cWehr, point );
          devider.setValue( values[i + 1] );
        }
    }
  }

  private boolean readWehr( final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbw = pr.getDataBlock( "OK-WEHR" ); //$NON-NLS-1$
    if( dbw == null )
      return false;

    final IProfileBuilding wehr = new BuildingWehr( p );
    final String secLine = dbw.getSecondLine();
    final String wehrart = getWehrart( secLine );
    final double[] wt = getWehrParameter( secLine );
    if( wehrart != null )
    {
      wehr.setValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART, wehrart );
    }

    wehr.setValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, wt == null ? 0.0 : wt[0] );
    p.addProfileObjects( new IProfileObject[] { wehr } );
    readWehrtrenner( wt, p, pr );
    final IComponent pp = p.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final double delta = pp == null ? 0.0001 : pp.getPrecision();
    final PolyLine polyLineO = new PolyLine( dbw.getX(), dbw.getY(), delta );
    final Range rangeO = new Range( polyLineO.getFirstX(), polyLineO.getLastX(), delta );
    for( final IRecord point : p.getPoints() )
    {
      final Double breite = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point );
      final Double hoehe = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, point );
      final int iOKW = p.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
      if( rangeO.contains( breite ) )
        point.setValue( iOKW, polyLineO.getYFor( breite ) );
      else
        point.setValue( iOKW, hoehe );

    }
    return true;
  }

  private double[] getWehrParameter( final String params )
  {
    final StringTokenizer sT = new StringTokenizer( params, " " ); //$NON-NLS-1$
    final int paramCount = sT.countTokens() - 1;
    if( paramCount < 1 )
      return null;
    // skip first parameter
    sT.nextToken();

    final double[] wp = new double[paramCount];
    for( int i = 0; i < paramCount; i++ )
      wp[i] = Double.parseDouble( sT.nextToken() );
    return wp;
  }

  private final String getWehrart( final String secLine )
  {
    final StringTokenizer sT = new StringTokenizer( secLine, " " ); //$NON-NLS-1$
    final int paramCount = sT.countTokens() - 1;
    if( paramCount < 0 )
      return null;
    final String wehrart = sT.nextToken().toUpperCase();

    if( wehrart.startsWith( "RUND" ) ) //$NON-NLS-1$
      return IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG;
    if( wehrart.startsWith( "BREI" ) ) //$NON-NLS-1$
      return IWspmTuhhConstants.WEHR_TYP_BREITKRONIG;
    if( wehrart.startsWith( "SCHA" ) ) //$NON-NLS-1$
      return IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG;
    if( wehrart.startsWith( "BEIW" ) ) //$NON-NLS-1$
      return IWspmTuhhConstants.WEHR_TYP_BEIWERT;

    return null;
  }

  private void readBordVoll( final IProfil p, final PrfReader pr )
  {
    final IRecord[] points = p.getPoints();
    if( points.length == 0 )
      return;
    final IDataBlock db = pr.getDataBlock( "BORDVOLL" ); //$NON-NLS-1$
    if( db == null )
      return;
    final int pCount = db.getCoordCount();
    IRecord p1 = null;
    IRecord p2 = null;

    if( pCount > 0 )
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
    if( pCount > 1 )
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
    if( pCount > 2 )
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.43", p.getStation() ) //$NON-NLS-1$
      , null ) ); //$NON-NLS-1$

    if( p1 != null )
    {
      final IProfilPointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, p1 );
      marker.setValue( true );
    }

    if( p2 != null )
    {
      final IProfilPointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, p2 );
      marker.setValue( true );
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSource#read(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public IProfil[] read( final String profileTyp, final Reader reader ) throws IOException
  {
    final IProfil profil = ProfilFactory.createProfil( profileTyp );

    if( profil == null )
      throw new IOException( Messages.getString("PrfSource.0") + profileTyp ); //$NON-NLS-1$

    final PrfReader prfReader = new PrfReader();
    prfReader.readFromReader( new BufferedReader( reader ) );
    readSource( prfReader, profil );
    return new IProfil[] { profil };
  }
}