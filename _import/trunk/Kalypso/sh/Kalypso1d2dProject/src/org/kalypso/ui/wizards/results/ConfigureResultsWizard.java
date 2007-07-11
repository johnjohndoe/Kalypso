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
package org.kalypso.ui.wizards.results;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

import de.renew.workflow.contexts.IDialogWithResult;

/**
 * @author GernotBelger
 */
public class ConfigureResultsWizard extends Wizard implements IWorkbenchWizard, IDialogWithResult
{
  private RestartSelectWizardPage m_restartSelectWizardPage;

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    setWindowTitle( "Ergebniskarte konfigurieren" );

    m_restartSelectWizardPage = new RestartSelectWizardPage( "" );
    m_restartSelectWizardPage.setTitle( "Ergebnisse auswählen" );
    m_restartSelectWizardPage.setDescription( "Hier können Sie ein oder mehrere Ergebnisse für die Ergebsnikarte auswählen." );

    addPage( m_restartSelectWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    // TODO Auto-generated method stub

    // add/remove selected results from/to map

    return false;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    // find currently open map
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext currentState = handlerService.getCurrentState();
    // final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) currentState.getVariable(
    // CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IWorkbenchPart part = (IWorkbenchPart) currentState.getVariable( ISources.ACTIVE_PART_NAME );
    // m_mapPart = (AbstractMapPart) part;
    final MapPanel panel = part == null ? null : (MapPanel) part.getAdapter( MapPanel.class );
    final IMapModell modell = panel == null ? null : panel.getMapModell();
    // final GisTemplateMapModell templateModell = (GisTemplateMapModell) modell;
    // find already existent results

  }

  /**
   * @see de.renew.workflow.contexts.IDialogWithResult#getResult()
   */
  public Object getResult( )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
