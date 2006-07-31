/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.io.PrintWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_TYP;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.wspwin.core.prf.PrfWriter;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.TextDataBlock;

/**
 * @author kimwerner
 */
public class PrfSink implements IProfilSink
{

  private final static Logger m_logger = Logger.getLogger( PrfSink.class.getName() );

  private void extractDataBlocks( final PrfWriter pw, final IProfil p )
  {
    final IProfilPoint anyPoint = p.getPoints().getFirst();
    writePoints( pw, p );
    writeDevider( pw, p );
    writeRauheit( pw, p );
    if( p.getBuilding() != null )
      writeBuilding( pw, p );
    if( anyPoint.hasProperty( POINT_PROPERTY.HOCHWERT ) )
      writeHochRechts( pw, p );
    if( anyPoint.hasProperty( POINT_PROPERTY.BEWUCHS_AX ) )
      writeBewuchs( pw, p );
  }

  private void writePoints( final PrfWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbh = PrfWriter.createHeader( "GEL" );
    final CoordDataBlock db = new CoordDataBlock( dbh );
    writeCoords( profil, POINT_PROPERTY.HOEHE, db );
    pw.addDataBlock( db );
  }

  private void writeRauheit( final PrfWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbhr = PrfWriter.createHeader( "RAU" );
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    writeCoords( profil, POINT_PROPERTY.RAUHEIT, dbr );
    final IProfilBuilding building = profil.getBuilding();
    if( building != null && building.getTyp() != BUILDING_TYP.BRUECKE && building.getTyp() != BUILDING_TYP.WEHR )
    {
      try
      {
        dbr.getY()[0] = (Double) building.getValueFor( BUILDING_PROPERTY.RAUHEIT );
      }
      catch( ProfilDataException e )
      {
        m_logger.log( Level.SEVERE, "Der Rauheitswert für das Bauwerk konnte nicht geschrieben werden." );
      }
    }
    pw.addDataBlock( dbr );
  }

  private void writeCoords( final IProfil profil, final POINT_PROPERTY prop, final CoordDataBlock db )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final double[] Xs = new double[points.size()];
    final double[] Ys = new double[points.size()];
    int index = 0;
    for( final IProfilPoint point : points )
    {
      try
      {
        Xs[index] = point.getValueFor( POINT_PROPERTY.BREITE );
        Ys[index] = point.hasProperty( prop ) ? point.getValueFor( prop ) : 0.0;
      }
      catch( ProfilDataException e )
      {
        Xs[index] = 0;
        Ys[index] = 0;
        m_logger.log( Level.SEVERE, prop.toString() + " an Position " + Integer.toString( index ) + " konnte nicht geschrieben werden." );
      }
      index++;
    }
    db.setCoords( Xs, Ys );
  }

  private void writeHochRechts( final PrfWriter pw, final IProfil profil )
  {

    final DataBlockHeader dbhh = PrfWriter.createHeader( "HOC" );
    final CoordDataBlock dbh = new CoordDataBlock( dbhh );
    writeCoords( profil, POINT_PROPERTY.HOCHWERT, dbh );
    pw.addDataBlock( dbh );

    final DataBlockHeader dbhr = PrfWriter.createHeader( "REC" );
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    writeCoords( profil, POINT_PROPERTY.RECHTSWERT, dbr );
    pw.addDataBlock( dbr );
  }

  private void writeDevider( final PrfWriter pw, final IProfil profil )
  {

    final IProfilDevider[] trennf = profil.getDevider( DEVIDER_TYP.TRENNFLAECHE );
    if( trennf != null && trennf.length > 0 )
    {
      final DataBlockHeader dbht = PrfWriter.createHeader( "TRENNF" );
      final CoordDataBlock dbt = new CoordDataBlock( dbht );
      final double[] xs = new double[trennf.length];
      final double[] ys = new double[trennf.length];
      int index = 0;
      for( IProfilDevider devider : trennf )
      {
        final IProfilPoint point = devider.getPoint();
        try
        {
          xs[index] = point.getValueFor( POINT_PROPERTY.BREITE );
        }
        catch( ProfilDataException e )
        {
          m_logger.log( Level.SEVERE, "Die Positionen der Trennflächen konnten nicht geschrieben werden." );

        }
        boolean isBoeschung = (Boolean) devider.getValueFor( DEVIDER_PROPERTY.BOESCHUNG );
        switch( index )
        {
          case 0:
            ys[0] = isBoeschung ? 1.0 : 3.0;
          case 1:
            ys[1] = isBoeschung ? 2.0 : 4.0;
          default:
            ys[index] = 0.0;
        }
        index++;
      }
      dbt.setCoords( xs, ys );
      pw.addDataBlock( dbt );
    }
    writeDeviderTyp( pw, "BOR", profil.getDevider( DEVIDER_TYP.BORDVOLL ) );
    writeDeviderTyp( pw, "DUR", profil.getDevider( DEVIDER_TYP.DURCHSTROEMTE ) );
    writeDeviderTyp( pw, "TRENNL", profil.getDevider( DEVIDER_TYP.WEHR ) );
  }

  private void writeDeviderTyp( final PrfWriter pw, final String key, IProfilDevider[] deviders )
  {

    if( deviders != null && deviders.length > 0 )
    {
      final DataBlockHeader dbhw = PrfWriter.createHeader( key );
      final CoordDataBlock dbw = new CoordDataBlock( dbhw );
      final double[] xs = new double[deviders.length];
      final double[] ys = new double[deviders.length];
      int index = 0;
      for( IProfilDevider devider : deviders )
      {
        final IProfilPoint point = devider.getPoint();
        try
        {
          xs[index] = point.getValueFor( POINT_PROPERTY.BREITE );
          ys[index] = point.getValueFor( POINT_PROPERTY.HOEHE );
        }
        catch( ProfilDataException e )
        {
          m_logger.log( Level.SEVERE, "Die Positionen der " + devider.getTyp().toString() + " konnten nicht geschrieben werden." );

        }
      }
      dbw.setCoords( xs, ys );
      pw.addDataBlock( dbw );
    }
  }

  private void writeBuilding( final PrfWriter pw, final IProfil profil )
  {
    final IProfilBuilding building = profil.getBuilding();
    switch( building.getTyp() )
    {
      case BRUECKE:
      {
        final DataBlockHeader dbho = PrfWriter.createHeader( "OK-B" );
        final CoordDataBlock dbo = new CoordDataBlock( dbho );
        writeCoords( profil, POINT_PROPERTY.OBERKANTEBRUECKE, dbo );
        pw.addDataBlock( dbo );
        final DataBlockHeader dbhu = PrfWriter.createHeader( "UK-B" );
        final CoordDataBlock dbu = new CoordDataBlock( dbhu );
        writeCoords( profil, POINT_PROPERTY.UNTERKANTEBRUECKE, dbu );
        try
        {
          final String secLine = String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.UNTERWASSER ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BREITE ) + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.RAUHEIT ) )
                  + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.FORMBEIWERT ) ) );
          dbu.setSecondLine( secLine );
        }
        catch( final ProfilDataException e )
        {
          m_logger.log( Level.SEVERE, "Fehler beim schreiben der Brückenparameter" );
        }
        pw.addDataBlock( dbu );
      }

      case WEHR:
      {
        final DataBlockHeader dbhw = PrfWriter.createHeader( "OK-W" );
        final CoordDataBlock dbw = new CoordDataBlock( dbhw );
        writeCoords( profil, POINT_PROPERTY.OBERKANTEWEHR, dbw );
        try
        {
          final StringBuffer secLine = new StringBuffer( building.getValueFor( BUILDING_PROPERTY.WEHRART ).toString().toUpperCase() );
          final IProfilDevider[] deviders = profil.getDevider( DEVIDER_TYP.WEHR );
          if( deviders != null )
          {
            for( IProfilDevider devider : deviders )
            {
              secLine.append( String.format( Locale.US, " %12.4f", devider.getValueFor( DEVIDER_PROPERTY.BEIWERT ) ) );
            }
          }
          dbw.setSecondLine( secLine.toString() );
        }
        catch( final ProfilDataException e )
        {
          m_logger.log( Level.SEVERE, "Fehler beim schreiben der Wehrparameter" );
        }
        pw.addDataBlock( dbw );
      }
      case EI:
      {
        final DataBlockHeader dbhe = PrfWriter.createHeader( "EI" );
        final TextDataBlock dbe = new TextDataBlock( dbhe );
        dbe.setThirdLine( "0  0  0  0  0  0  0  0  8" );
        try
        {
          dbe.addLine( String.format( Locale.US, " %12.4f", (building.getValueFor( BUILDING_PROPERTY.BREITE )) + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.HOEHE ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.SOHLGEFAELLE ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_X ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_Y ) ) ) );
        }
        catch( final ProfilDataException e )
        {
          m_logger.log( Level.SEVERE, "Fehler beim schreiben der Bauwerksparameter" );
        }
        pw.addDataBlock( dbe );
      }
      case MAUL:
      {
        final DataBlockHeader dbhm = PrfWriter.createHeader( "MAU" );
        final TextDataBlock dbm = new TextDataBlock( dbhm );
        dbm.setThirdLine( "0  0  0  0  0  0  0  0  9" );
        try
        {
          dbm.addLine( String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BREITE ) ) + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.HOEHE ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.SOHLGEFAELLE ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_X ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_Y ) ) );
        }
        catch( final ProfilDataException e )
        {
          m_logger.log( Level.SEVERE, "Fehler beim schreiben der Bauwerksparameter" );
        }
        pw.addDataBlock( dbm );
      }
      case KREIS:
      {
        final DataBlockHeader dbhk = PrfWriter.createHeader( "KRE" );
        final TextDataBlock dbk = new TextDataBlock( dbhk );
        dbk.setThirdLine( "0  0  0  0  0  0  0  0  7" );
        try
        {
          dbk.addLine( String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BREITE ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.SOHLGEFAELLE ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_X ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_Y ) ) );
        }
        catch( final ProfilDataException e )
        {
          m_logger.log( Level.SEVERE, "Fehler beim schreiben der Bauwerksparameter" );
        }
        pw.addDataBlock( dbk );
      }
      case TRAPEZ:
      {
        final DataBlockHeader dbht = PrfWriter.createHeader( "TRA" );
        final TextDataBlock dbt = new TextDataBlock( dbht );
        dbt.setThirdLine( "0  0  0  0  0  0  0  0  6" );
        try
        {
          dbt.addLine( String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BREITE ) ) + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.HOEHE ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.STEIGUNG ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.SOHLGEFAELLE ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_X ) )
              + String.format( Locale.US, " %12.4f", building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_Y ) ) );
        }
        catch( final ProfilDataException e )
        {
          m_logger.log( Level.SEVERE, "Fehler beim schreiben der Bauwerksparameter" );
        }
        pw.addDataBlock( dbt );
      }
    }
    if( building == null )
      return;
  }

  private void writeBewuchs( final PrfWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbhx = PrfWriter.createHeader( "AX" );
    final CoordDataBlock dbx = new CoordDataBlock( dbhx );
    final DataBlockHeader dbhy = PrfWriter.createHeader( "AY" );
    final CoordDataBlock dby = new CoordDataBlock( dbhy );
    final DataBlockHeader dbhp = PrfWriter.createHeader( "DP" );
    final CoordDataBlock dbp = new CoordDataBlock( dbhp );
    writeCoords( profil, POINT_PROPERTY.BEWUCHS_AX, dbx );
    writeCoords( profil, POINT_PROPERTY.BEWUCHS_AY, dby );
    writeCoords( profil, POINT_PROPERTY.BEWUCHS_DP, dbp );
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
    }
    catch( Exception e )
    {
      metaData = new HashMap<Integer, String[]>();
    }
    final String[] line1 = metaData.get( 1 );
    if( line1 == null )
    {
      pw.addKeyValue( 1, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 1, line1 );
    }
    final String[] line2 = metaData.get( 2 );
    if( line2 == null )
    {
      pw.addKeyValue( 2, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 2, line2 );
    }
    final String[] line3 = metaData.get( 3 );
    if( line3 == null )
    {
      pw.addKeyValue( 3, new String[] { "STATUS", "" } );
    }
    else
    {
      if( line3[0].length() == 0 )
        line3[0] = "STATUS";
      pw.addKeyValue( 3, line3 );
    }
    final String[] line4 = metaData.get( 4 );
    if( line4 == null )
    {
      pw.addKeyValue( 4, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 4, line4 );
    }
    final String[] line5 = metaData.get( 5 );
    if( line5 == null )
    {
      pw.addKeyValue( 5, new String[] { "VERZWEIGUNGSKENNUNG", "0" } );
    }
    else
    {
      if( line5[0].length() == 0 )
        line5[0] = "VERZWEIGUNGSKENNUNG";
      pw.addKeyValue( 5, line5 );
    }
    final String[] line6 = metaData.get( 6 );
    if( line6 == null )
    {
      pw.addKeyValue( 6, new String[] { "WASSERSPIEGEL", "" } );
    }
    else
    {
      if( line6[0].length() == 0 )
        line6[0] = "WASSERSPIEGEL";
      pw.addKeyValue( 6, line6 );
    }
    final String[] line7 = metaData.get( 7 );
    if( line7 == null )
    {
      pw.addKeyValue( 7, new String[] { "MEHRFELDBRUECKE", "0" } );
    }
    else
    {
      if( line7[0].length() == 0 )
        line7[0] = "MEHRFELDBRUECKE";
      pw.addKeyValue( 7, line7 );
    }
    final String[] line8 = metaData.get( 8 );
    if( line8 == null )
    {
      pw.addKeyValue( 8, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 8, line8 );
    }
    final Double d = p.getStation();
    if( d.isNaN() )
      pw.addKeyValue( 9, new String[] { "STATION KM 0.0000", "" } );
    else
      pw.addKeyValue( 9, new String[] { "STATION KM " + String.format( Locale.US, "%.4f", new Object[] { d } ), "" } );

    final String[] line10 = metaData.get( 10 );
    if( line10 == null )
    {
      pw.addKeyValue( 10, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 10, line10 );
    }
    final String[] line11 = metaData.get( 11 );
    if( line11 == null )
    {
      pw.addKeyValue( 11, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 11, line11 );
    }
    final String[] line12 = metaData.get( 12 );
    if( line12 == null )
    {
      pw.addKeyValue( 12, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 12, line12 );
    }
    final String[] line13 = metaData.get( 13 );
    if( line13 == null )
    {
      pw.addKeyValue( 13, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 13, line13 );
    }

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void write( final IProfil profil, final Writer writer )
  {
    final PrintWriter pw = new PrintWriter( writer );

    final PrfWriter prfwriter = new PrfWriter();
    extractMetaData( prfwriter, profil );
    if( !profil.getPoints().isEmpty() )
      extractDataBlocks( prfwriter, profil );
    prfwriter.store( pw );
  }

}
