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
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.ui.view.table.handler.VegetationClassComparator;

/**
 * Writes the vegetation classes to a file.
 * 
 * @author Holger Albert
 */
public class BwpWriter
{
  /**
   * The constructor.
   */
  public BwpWriter( )
  {
  }

  public IStatus write( final IVegetationClass[] vegetationClasses, final File file, IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The writer. */
    PrintWriter writer = null;

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString("BwpWriter_0"), 100 * vegetationClasses.length ); //$NON-NLS-1$
      monitor.subTask( Messages.getString("BwpWriter_1") ); //$NON-NLS-1$

      /* Create the writer. */
      writer = new PrintWriter( file );

      /* Write the header line. */
      writer.println( "KENN\tdp\tax\tay\t\"Bewuchsdefinition\"" ); //$NON-NLS-1$

      /* Loop the vegetation classes. */
      Arrays.sort( vegetationClasses, new VegetationClassComparator() );
      for( final IVegetationClass vegetationClass : vegetationClasses )
      {
        /* Write the vegetation class. */
        writeVetegationClass( vegetationClass, writer );

        /* Monitor. */
        monitor.worked( 100 );
      }

      return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString("BwpWriter_3") ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, String.format( Messages.getString("BwpWriter_4"), ex.getLocalizedMessage() ), ex ); //$NON-NLS-1$
    }
    finally
    {
      /* Close the writer. */
      IOUtils.closeQuietly( writer );

      /* Monitor. */
      monitor.done();
    }
  }

  private void writeVetegationClass( final IVegetationClass vegetationClass, final PrintWriter writer )
  {
    /* Create the line. */
    final StringBuilder builder = new StringBuilder();
    builder.append( "%s\t" ); //$NON-NLS-1$
    builder.append( "%s\t" ); //$NON-NLS-1$
    builder.append( "%s\t" ); //$NON-NLS-1$
    builder.append( "%s\t" ); //$NON-NLS-1$
    builder.append( "\"%s\"" ); //$NON-NLS-1$

    /* Get the data to write. */
    final String name = vegetationClass.getName();

    String dp = "-1"; //$NON-NLS-1$
    final BigDecimal dp1 = vegetationClass.getDp();
    if( dp1 != null )
      dp = String.format( Locale.PRC, "%.3f", dp1.doubleValue() ); //$NON-NLS-1$

    String ax = "-1"; //$NON-NLS-1$
    final BigDecimal ax1 = vegetationClass.getAx();
    if( ax1 != null )
      ax = String.format( Locale.PRC, "%.3f", ax1.doubleValue() ); //$NON-NLS-1$

    String ay = "-1"; //$NON-NLS-1$
    final BigDecimal ay1 = vegetationClass.getAy();
    if( ay1 != null )
      ay = String.format( Locale.PRC, "%.3f", ay1.doubleValue() ); //$NON-NLS-1$

    String comment = ""; //$NON-NLS-1$
    final String comment1 = vegetationClass.getComment();
    if( comment1 != null )
      comment = comment1;

    /* Write the line. */
    writer.format( Locale.PRC, builder.toString(), name, dp, ax, ay, comment );
    writer.println();
  }
}