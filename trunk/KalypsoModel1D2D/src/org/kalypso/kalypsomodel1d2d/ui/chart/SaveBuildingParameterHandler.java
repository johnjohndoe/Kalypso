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
package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;

import de.openali.odysseus.chart.framework.view.IChartComposite;

/**
 * Saves the changes made on a building into the real flow relation.
 * 
 * @author Gernot Belger
 */
public class SaveBuildingParameterHandler extends AbstractHandler implements IElementUpdater
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext)event.getApplicationContext();
    final Shell shell = (Shell)context.getVariable( ISources.ACTIVE_SHELL_NAME );

    final IChartComposite chart = ChartHandlerUtilities.getChartChecked( context );
    final IChartPart part = ChartHandlerUtilities.findChartComposite( context );

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
      {
        final BuildingParameterLayer layer = EditBuildingParameterMouseHandler.findLayer( chart.getChartModel() );
        if( layer != null )
          layer.saveData( monitor );

        return Status.OK_STATUS;
      }
    };

    final IStatus status = ProgressUtilities.busyCursorWhile( operation );

    // REMARK: must be called in ui thread!
    ChartHandlerUtilities.updateElements( part );

    if( !status.isOK() )
    {
      final String commandName = HandlerUtils.getCommandName( event );
      StatusDialog.open( shell, status, commandName );
    }

    return null;
  }

  @Override
  public void updateElement( final UIElement element, final Map parameters )
  {
    // TODO: disable/enable according to dirty state of layer; this is tricky, as it is hard to find the layer here...
    // ElementUpdateHelper.updateElement( element, parameters, RemoveBuildingParameterMouseHandler.class );
  }
}
