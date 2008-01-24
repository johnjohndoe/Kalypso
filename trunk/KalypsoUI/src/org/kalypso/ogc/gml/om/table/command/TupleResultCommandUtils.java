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
package org.kalypso.ogc.gml.om.table.command;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.observation.result.TupleResult;

/**
 * Utility methods for tuple-result command handlers.
 * 
 * @author Gernot Belger
 */
public class TupleResultCommandUtils
{
  public final static String ACTIVE_TUPLE_RESULT_TABLE_VIEWER_NAME = "tupleResultTableViewer";

  public static final String TUPLE_RESULT_COMMAND_CATEGORY = "org.kalypso.ui.tupleResult.category";

  private TupleResultCommandUtils( )
  {
    throw new UnsupportedOperationException( "Do not instantiate this helper class." );
  }

  /**
   * Helps finding the chart composite in the context.<br>
   * Normally (for editor and view) this is done via adapting the active workbench part.<br>
   * However for the feature view some hack was needed: here it is found via the activeChartComposite variable.
   */
  public static TableViewer findTableViewer( final ExecutionEvent event )
  {
    final Object applicationContext = event.getApplicationContext();
    if( !(applicationContext instanceof IEvaluationContext) )
      return null;

    final IEvaluationContext context = (IEvaluationContext) applicationContext;

    final Object variable = context.getVariable( ACTIVE_TUPLE_RESULT_TABLE_VIEWER_NAME );
    if( variable instanceof TableViewer )
      return (TableViewer) variable;

    final IWorkbenchPart activePart = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    if( activePart == null )
      return null;

    final ITupleResultViewerProvider provider = (ITupleResultViewerProvider) activePart.getAdapter( ITupleResultViewerProvider.class );
    if( provider == null )
      return null;

    return provider.getTupleResultViewer();
  }

  public static TupleResult findTupleResult( final ExecutionEvent event )
  {
    final Object applicationContext = event.getApplicationContext();
    if( !(applicationContext instanceof IEvaluationContext) )
      return null;

    final IEvaluationContext context = (IEvaluationContext) applicationContext;

    final Object variable = context.getVariable( ACTIVE_TUPLE_RESULT_TABLE_VIEWER_NAME );
    if( variable instanceof TableViewer )
    {
      final TableViewer tableViewer = (TableViewer) variable;
      final Object input = tableViewer.getInput();
      if( input instanceof TupleResult )
        return (TupleResult) input;

      return null; // do not continue if we already could find the viewer
    }

    final IWorkbenchPart activePart = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    if( activePart == null )
      return null;

    final ITupleResultViewerProvider provider = (ITupleResultViewerProvider) activePart.getAdapter( ITupleResultViewerProvider.class );
    if( provider == null )
      return null;

    return provider.getTupleResult();
  }

}
