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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Locale;

import junit.framework.TestCase;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.devider.ProfilDevider;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink;

/**
 * @author kimwerner
 */
public class CreateProfilesTest extends TestCase
{
  final String dir = "c:\\PrfTest\\";

  public void testRunTest( ) throws Exception
  {
    final ArrayList<IProfil> p = new ArrayList<IProfil>();

    p.add( createMinProf() );
    p.add(createWspWinProf());
    writePrf( p );
    writeStr( p );

  }
public IProfil createWspWinProf()
{
  final IProfil p = createMinProf() ;
  p.addPointProperty( POINT_PROPERTY.RAUHEIT);
  
  p.addDevider(p.getPoints().get(0),IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE);
  p.addDevider(p.getPoints().get(4),IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE);
  p.addDevider(p.getPoints().get(1),IProfilDevider.DEVIDER_TYP.TRENNFLAECHE);
  p.addDevider(p.getPoints().get(3),IProfilDevider.DEVIDER_TYP.TRENNFLAECHE);
  
  return p;
}
  public void writePrf( ArrayList<IProfil> profs ) throws FileNotFoundException
  {

    final IProfilSink ps = new PrfSink();
    Integer nr = 0;
    for( IProfil p : profs )
    {
      p.setStation( nr );
      final File outPrfFile = new File( dir + String.format( Locale.US, "%04o", nr++ ) + ".prf" );
      final PrintWriter prfWriter = new PrintWriter( outPrfFile );
      ps.write( p, prfWriter );
    }
  }

  public void writeStr( ArrayList<IProfil> profs ) throws IOException
  {
    final FileWriter f = new FileWriter( dir + "ProfilTest.str" );
    final Integer cnt = profs.size();
    
    f.write( String.format( Locale.US, "%5o", cnt ) + String.format( Locale.US, "%5o", cnt - 1 ) + "      Test       Prf" );
    for( Integer stat = 0;stat < cnt;stat++ )
    {
      f.write( "\nTest" + String.format( Locale.US, "%10o", stat ) + " 0         0   Ist        " + String.format( Locale.US, "%04o", stat ) + ".prf" );
    }
    f.write("\n\n");
    for( Integer stat = 1;stat < cnt;stat++ )
    {
      f.write(  String.format( Locale.US, "%.6f", new Double(stat-1 )) + String.format( Locale.US, "% .6f", new Double(stat) )+" 1.000 1.000 1.000 " + String.format( Locale.US, "%04o", stat-1 ) + ".prf " + String.format( Locale.US, "%04o", stat ) + ".prf");
    }
    
    f.flush();
    f.close();
  }

  public IProfil createMinProf( )
  {
    final IProfil p = ProfilFactory.createProfil();
    p.addPoint( 0, 100 );
    p.addPoint( 15, 70 );
    p.addPoint( 35, 40 );
    p.addPoint( 50, 20 );
    p.addPoint( 65, 40 );
    p.addPoint( 85, 70 );
    p.addPoint( 100, 100 );
    return p;
  }
}
