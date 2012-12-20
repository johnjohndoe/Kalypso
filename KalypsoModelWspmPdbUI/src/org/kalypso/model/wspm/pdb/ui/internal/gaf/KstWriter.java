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
package org.kalypso.model.wspm.pdb.ui.internal.gaf;

import java.io.File;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Locale;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.ui.view.table.handler.RoughnessClassComparator;

/**
 * Writes the roughness classes to a file.
 * 
 * @author Holger Albert
 */
public class KstWriter
{
  /**
   * The constructor.
   */
  public KstWriter( )
  {
  }

  public IStatus write( final IRoughnessClass[] roughnessClasses, final File file, IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The writer. */
    PrintWriter writer = null;

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString("KstWriter_0"), 100 * roughnessClasses.length ); //$NON-NLS-1$
      monitor.subTask( Messages.getString("KstWriter_1") ); //$NON-NLS-1$

      /* Create the writer. */
      writer = new PrintWriter( file );

      /* Write the header line. */
      writer.println( "KENN\tm\tm^.33/s\t\"Oberfl‰chenstruktur\"" ); //$NON-NLS-1$

      /* Loop the roughness classes. */
      Arrays.sort( roughnessClasses, new RoughnessClassComparator() );
      for( final IRoughnessClass roughnessClass : roughnessClasses )
      {
        /* Write the roughness class. */
        writeRoughnessClass( roughnessClass, writer );

        /* Monitor. */
        monitor.worked( 100 );
      }

      return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString("KstWriter_3") ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, String.format( Messages.getString("KstWriter_4"), ex.getLocalizedMessage() ), ex ); //$NON-NLS-1$
    }
    finally
    {
      /* Close the writer. */
      IOUtils.closeQuietly( writer );

      /* Monitor. */
      monitor.done();
    }
  }

  private void writeRoughnessClass( final IRoughnessClass roughnessClass, final PrintWriter writer )
  {
    /* Create the line. */
    final StringBuilder builder = new StringBuilder();
    builder.append( "%s\t" ); //$NON-NLS-1$
    builder.append( "%s\t" ); //$NON-NLS-1$
    builder.append( "%s\t" ); //$NON-NLS-1$
    builder.append( "\"%s\"" ); //$NON-NLS-1$

    /* Get the data to write. */
    final String name = roughnessClass.getName();

    String ksValue = "-1"; //$NON-NLS-1$
    final BigDecimal ksValue1 = roughnessClass.getKsValue();
    if( ksValue1 != null )
      ksValue = String.format( Locale.PRC, "%.3f", ksValue1.doubleValue() ); //$NON-NLS-1$

    String kstValue = "-1"; //$NON-NLS-1$
    final BigDecimal kstValue1 = roughnessClass.getKstValue();
    if( kstValue1 != null )
      kstValue = String.format( Locale.PRC, "%.1f", kstValue1.doubleValue() ); //$NON-NLS-1$

    String comment = ""; //$NON-NLS-1$
    final String comment1 = roughnessClass.getComment();
    if( comment1 != null )
      comment = comment1;

    /* Write the line. */
    writer.format( Locale.PRC, builder.toString(), name, ksValue, kstValue, comment );
    writer.println();
  }
}