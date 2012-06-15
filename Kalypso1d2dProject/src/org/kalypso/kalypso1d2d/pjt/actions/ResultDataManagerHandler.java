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
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.ui.wizards.results.ResultManager1d2dWizard;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * @author Thomas Jung
 */
public class ResultDataManagerHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );

    /* Get the map */
    final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.2" ) ); //$NON-NLS-1$
    final JobExclusiveCommandTarget commandTarget = mapView.getCommandTarget();

    final IMapPanel mapPanel = mapView.getMapPanel();

    // wait until map has loaded
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ResultDataManagerHandler.0" ), Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ResultDataManagerHandler.1" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return null;

    // Open wizard on that map!
    final ResultManager1d2dWizard managerWizard = new ResultManager1d2dWizard();
    managerWizard.init( PlatformUI.getWorkbench(), new StructuredSelection() );
    managerWizard.setCommandTarget( commandTarget );
    managerWizard.setMapModel( (IKalypsoLayerModell) mapPanel.getMapModell() );

    final WizardDialog2 wizardDialog2 = new WizardDialog2( shell, managerWizard );
    if( wizardDialog2.open() == Window.OK )
      return Status.OK_STATUS;

    return Status.CANCEL_STATUS;
  }

}
