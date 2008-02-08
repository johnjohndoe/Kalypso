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

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.wspwin.core.prf.PrfWriter;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.TextDataBlock;

/**
 * @author kimwerner
 */
public class PrfSink implements IProfilSink
{

  private String toDataBlockKey( final Object profilKey )
  {

    if( IWspmTuhhConstants.WEHR_TYP_BEIWERT.equals( profilKey ) )
      return "BEIWERT";
    else if( IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG.equals( profilKey ) )
      return "RUNDKRONIG";
    else if( IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG.equals( profilKey ) )
      return "SCHARFKANTIG";
    else if( IWspmTuhhConstants.WEHR_TYP_BREITKRONIG.equals( profilKey ) )
      return "BREITKRONIG";
    else
      return profilKey.toString();
  }

  private void extractDataBlocks( final PrfWriter pw, final IProfil p )
  {
    writePoints( pw, p );
    writeDevider( pw, p );
    writeRauheit( pw, p );
    if( p.getProfileObjects() != null )
      writeBuilding( pw, p );
    if( p.hasPointProperty( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_HOCHWERT ) ) )
      writeHochRechts( pw, p );
    if( p.hasPointProperty( ProfilObsHelper.getPropertyFromId( p, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) ) )
      writeBewuchs( pw, p );
    if( p.getComment() != null )
      writeComment( pw, p );
  }

  private void writeComment( final PrfWriter pw, final IProfil profil )
  {
    final String comment = profil.getComment();
    final DataBlockHeader dbh = PrfWriter.createHeader( "KOM" );
    final TextDataBlock db = new TextDataBlock( dbh );

    final StringReader stringReader = new StringReader( comment );
    final LineNumberReader lineNumberReader = new LineNumberReader( stringReader );

    try
    {
      for( String line = lineNumberReader.readLine(); line != null; line = lineNumberReader.readLine() )
        db.addLine( "CC " + line );
      if( db.getCoordCount() > 0 )
      {
        db.setThirdLine( "0  0  0  0  0  0  0  " + Integer.toString( db.getCoordCount() ) + " 17" );
        pw.addDataBlock( db );
      }
    }
    catch( final IOException e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.WARNING, "", 0, "Fehler beim schreiben des Kommentars", e ) );
    }
  }

  private void writePoints( final PrfWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbh = PrfWriter.createHeader( "GEL" );
    final CoordDataBlock db = new CoordDataBlock( dbh );
    writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_HOEHE ), db );
    pw.addDataBlock( db );
  }

  private void writeRauheit( final PrfWriter pw, final IProfil profil )
  {
    CoordDataBlock dbr = null;
    if( profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) ) )
    {
      final DataBlockHeader dbhr = PrfWriter.createHeader( "KS" );
      dbr = new CoordDataBlock( dbhr );
      writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ), dbr );
    }
    else if( profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) ) )
    {
      final DataBlockHeader dbhr = PrfWriter.createHeader( "KST" );
      dbr = new CoordDataBlock( dbhr );
      writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ), dbr );
    }
    else
      return;

    final IProfileObject[] buildings = profil.getProfileObjects();

    IProfileObject building = null;
    if( buildings.length > 0 )
      building = buildings[0];

   // final String buildingTyp = building == null ? "" : building.getId();
    if(building !=null&& !IWspmTuhhConstants.BUILDING_TYP_BRUECKE.equals( building.getId() )&&!IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( building.getId() ) )
      try
      {
        dbr.getY()[0] = (Double) building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.WARNING, "", 0, "Fehler beim schreiben der Rauheitswerte f�r " + building.getId() + ".", e ) );
      }
    pw.addDataBlock( dbr );
  }

  private void writeCoords( final IProfil profil, final IComponent prop, final CoordDataBlock db )
  {
    final IRecord[] points = profil.getPoints();
    final double[] Xs = new double[points.length];
    final double[] Ys = new double[points.length];
    int index = 0;
    for( final IRecord point : points )
    {
      try
      {
        Xs[index] = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_BREITE ) );
        Ys[index] = profil.hasPointProperty( prop ) ? (Double) point.getValue( prop ) : 0.0;
      }
      catch( final Exception e )
      {
        Xs[index] = 0;
        Ys[index] = 0;
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, prop + " an Position " + Integer.toString( index ) + " konnte nicht geschrieben werden.", e ) );
      }
      index++;
    }
    db.setCoords( Xs, Ys );
  }

  private void writeHochRechts( final PrfWriter pw, final IProfil profil )
  {

    final DataBlockHeader dbhh = PrfWriter.createHeader( "HOC" );
    final CoordDataBlock dbh = new CoordDataBlock( dbhh );
    writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_HOCHWERT ), dbh );
    pw.addDataBlock( dbh );

    final DataBlockHeader dbhr = PrfWriter.createHeader( "REC" );
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_RECHTSWERT ), dbr );
    pw.addDataBlock( dbr );
  }

  private void writeDevider( final PrfWriter pw, final IProfil profil )
  {
    writeDeviderTyp( pw, "TRENNF", profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) ) );
    writeDeviderTyp( pw, "BOR", profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) ) );
    writeDeviderTyp( pw, "DUR", profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) ) );
    writeDeviderTyp( pw, "TRENNL", profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_WEHR ) ) );
  }

  private void writeDeviderTyp( final PrfWriter pw, final String key, final IProfilPointMarker[] deviders )
  {
    if( deviders == null || deviders.length == 0 )
      return;

    final CoordDataBlock dbw = new CoordDataBlock( PrfWriter.createHeader( key ) );
    final double[] xs = new double[deviders.length];
    final double[] ys = new double[deviders.length];

    for( int i = 0; i < deviders.length; i++ )
    {
      final IProfilPointMarker devider = deviders[i];
      final IRecord point = devider.getPoint();

      try
      {
        xs[i] = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) );
        ys[i] = getDeviderYValue( devider, i );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, "Die Positionen der " + devider.getId().toString() + " konnten nicht geschrieben werden.", e ) );
      }
    }
    dbw.setCoords( xs, ys );
    pw.addDataBlock( dbw );
  }

  /**
   * Get coodinate y value: depends on type of marker.
   */
  private double getDeviderYValue( final IProfilPointMarker devider, final int index )
  {
    final IComponent markerId = devider.getId();

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId.getId() ) )
    {
      final boolean isBoeschung = devider.getIntepretedValue() == null ? false : (Boolean) devider.getIntepretedValue();
      final int offset = isBoeschung ? 3 : 1;
      return offset + index;
    }

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId.getId() ) )
      return index + 1;

    return (Double) devider.getPoint().getValue( ProfilObsHelper.getPropertyFromId( devider.getPoint(), IWspmConstants.POINT_PROPERTY_HOEHE ) );
  }

  private void writeBuilding( final PrfWriter pw, final IProfil profil )
  {
    final IProfileObject[] buildings = profil.getProfileObjects();

    // FIXME getter is now a list
    IProfileObject building = null;
    if( buildings.length > 0 )
      building = buildings[0];

    final String buildingTyp = building == null ? "" : building.getId();
    if( buildingTyp.equals( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) )
    {
      final DataBlockHeader dbho = PrfWriter.createHeader( "OK-B" );
      final CoordDataBlock dbo = new CoordDataBlock( dbho );
      writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ), dbo );
      pw.addDataBlock( dbo );
      final DataBlockHeader dbhu = PrfWriter.createHeader( "UK-B" );
      final CoordDataBlock dbu = new CoordDataBlock( dbhu );
      writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ), dbu );
      try
      {
        final String secLine = String.format( Locale.US, " %12.4f", building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_UNTERWASSER ) ) )
            + String.format( Locale.US, " %12.4f", building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            + String.format( Locale.US, " %12.4f", building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ) ) )
            + String.format( Locale.US, " %12.4f", building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) ) );
        dbu.setSecondLine( secLine );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, "Fehler beim schreiben der Br�ckenparameter", e ) );
      }
      pw.addDataBlock( dbu );
    }

    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_WEHR ) == 0 )
    {
      final DataBlockHeader dbhw = PrfWriter.createHeader( "OK-W" );
      final CoordDataBlock dbw = new CoordDataBlock( dbhw );
      writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ), dbw );
      try
      {
        final Object wehrart = building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART ) );

        final StringBuffer secLine = new StringBuffer( toDataBlockKey( wehrart ) );
        secLine.append( String.format( Locale.US, " %12.4f", building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) ) ) );
        final IProfilPointMarker[] deviders = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_WEHR ) );
        for( final IProfilPointMarker devider : deviders )
          secLine.append( String.format( Locale.US, " %12.4f", devider.getValue() ) );
        dbw.setSecondLine( secLine.toString() );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, "Fehler beim schreiben der Wehrparameter", e ) );
      }
      pw.addDataBlock( dbw );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_EI ) == 0 )
    {
      final DataBlockHeader dbhe = PrfWriter.createHeader( "EI" );
      final TextDataBlock dbe = new TextDataBlock( dbhe );
      dbe.setThirdLine( "0  0  0  0  0  0  0  0  8" );
      try
      {
        dbe.addLine( getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, "Fehler beim schreiben der Bauwerksparameter", e ) );
      }
      pw.addDataBlock( dbe );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_MAUL ) == 0 )
    {
      final DataBlockHeader dbhm = PrfWriter.createHeader( "MAU" );
      final TextDataBlock dbm = new TextDataBlock( dbhm );
      dbm.setThirdLine( "0  0  0  0  0  0  0  0  9" );
      try
      {
        dbm.addLine( getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, "Fehler beim schreiben der Bauwerksparameter", e ) );
      }
      pw.addDataBlock( dbm );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_KREIS ) == 0 )
    {
      final DataBlockHeader dbhk = PrfWriter.createHeader( "KRE" );
      final TextDataBlock dbk = new TextDataBlock( dbhk );
      dbk.setThirdLine( "0  0  0  0  0  0  0  0  7" );
      try
      {
        dbk.addLine( getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, "Fehler beim schreiben der Bauwerksparameter", e ) );
      }
      pw.addDataBlock( dbk );
    }

    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) == 0 )
    {
      final DataBlockHeader dbht = PrfWriter.createHeader( "TRA" );
      final TextDataBlock dbt = new TextDataBlock( dbht );
      dbt.setThirdLine( "0  0  0  0  0  0  0  0  6" );
      try
      {
        dbt.addLine( getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) )
            + getDoubleStr( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, "Fehler beim schreiben der Bauwerksparameter", e ) );
      }
      pw.addDataBlock( dbt );
    }
  }

  private String getDoubleStr( final Object o )
  {

    try
    {
      return Double.valueOf( o.toString() ).isNaN() ? "       0.0000" : String.format( Locale.US, " %12.4f", o );
    }
    catch( final Exception e )
    {
      return "       0.0000";
    }
  }

  private void writeBewuchs( final PrfWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbhx = PrfWriter.createHeader( "AX" );
    final CoordDataBlock dbx = new CoordDataBlock( dbhx );
    final DataBlockHeader dbhy = PrfWriter.createHeader( "AY" );
    final CoordDataBlock dby = new CoordDataBlock( dbhy );
    final DataBlockHeader dbhp = PrfWriter.createHeader( "DP" );
    final CoordDataBlock dbp = new CoordDataBlock( dbhp );
    writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ), dbx );
    writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ), dby );
    writeCoords( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ), dbp );
    pw.addDataBlock( dbx );
    pw.addDataBlock( dby );
    pw.addDataBlock( dbp );
  }

  @SuppressWarnings("unchecked")
  private void extractMetaData( final PrfWriter pw, final IProfil p )

  {

    Map<Integer, String[]> metaData;
    try
    {
      metaData = (Map<Integer, String[]>) p.getProperty( "prfFileFormat_MetaData" );
      if( metaData == null )
        metaData = new HashMap<Integer, String[]>();
    }
    catch( final Exception e )
    {
      metaData = new HashMap<Integer, String[]>();
    }
    final String[] line1 = metaData.get( 1 );
    if( line1 == null )
      pw.addKeyValue( 1, new String[] { "", "" } );
    else
      pw.addKeyValue( 1, line1 );
    final String[] line2 = metaData.get( 2 );
    if( line2 == null )
      pw.addKeyValue( 2, new String[] { "", "" } );
    else
      pw.addKeyValue( 2, line2 );
    final String[] line3 = metaData.get( 3 );
    if( line3 == null )
      pw.addKeyValue( 3, new String[] { "STATUS", "" } );
    else
    {
      if( line3[0].length() == 0 )
        line3[0] = "STATUS";
      pw.addKeyValue( 3, line3 );
    }
    final String[] line4 = metaData.get( 4 );
    if( line4 == null )
      pw.addKeyValue( 4, new String[] { "", "" } );
    else
      pw.addKeyValue( 4, line4 );
    final String[] line5 = metaData.get( 5 );
    if( line5 == null )
      pw.addKeyValue( 5, new String[] { "VERZWEIGUNGSKENNUNG", "0" } );
    else
    {
      if( line5[0].length() == 0 )
        line5[0] = "VERZWEIGUNGSKENNUNG";
      pw.addKeyValue( 5, line5 );
    }
    final String[] line6 = metaData.get( 6 );
    if( line6 == null )
      pw.addKeyValue( 6, new String[] { "WASSERSPIEGEL", "" } );
    else
    {
      if( line6[0].length() == 0 )
        line6[0] = "WASSERSPIEGEL";
      pw.addKeyValue( 6, line6 );
    }
    final String[] line7 = metaData.get( 7 );
    if( line7 == null )
      pw.addKeyValue( 7, new String[] { "MEHRFELDBRUECKE", "0" } );
    else
    {
      if( line7[0].length() == 0 )
        line7[0] = "MEHRFELDBRUECKE";
      pw.addKeyValue( 7, line7 );
    }
    final String[] line8 = metaData.get( 8 );
    if( line8 == null )
      pw.addKeyValue( 8, new String[] { "", "" } );
    else
      pw.addKeyValue( 8, line8 );
    final Double d = p.getStation();
    if( d.isNaN() )
      pw.addKeyValue( 9, new String[] { "STATION KM 0.0000", "" } );
    else
      pw.addKeyValue( 9, new String[] { "STATION KM " + String.format( Locale.US, "%.4f", new Object[] { d } ), "" } );

    final String[] line10 = metaData.get( 10 );
    if( line10 == null )
      pw.addKeyValue( 10, new String[] { "", "" } );
    else
      pw.addKeyValue( 10, line10 );
    final String[] line11 = metaData.get( 11 );
    if( line11 == null )
      pw.addKeyValue( 11, new String[] { "", "" } );
    else
      pw.addKeyValue( 11, line11 );
    final String[] line12 = metaData.get( 12 );
    if( line12 == null )
      pw.addKeyValue( 12, new String[] { "", "" } );
    else
      pw.addKeyValue( 12, line12 );
    final String[] line13 = metaData.get( 13 );
    if( line13 == null )
      pw.addKeyValue( 13, new String[] { "", "" } );
    else
      pw.addKeyValue( 13, line13 );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void write( final IProfil profil, final Writer writer )
  {
    final PrintWriter pw = new PrintWriter( writer );

    final PrfWriter prfwriter = new PrfWriter();
    extractMetaData( prfwriter, profil );
    if( profil.getPoints().length > 0 )
      extractDataBlocks( prfwriter, profil );
    prfwriter.store( pw );
  }

}
