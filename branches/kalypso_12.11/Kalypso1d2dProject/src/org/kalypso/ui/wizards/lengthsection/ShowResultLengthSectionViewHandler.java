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
package org.kalypso.ui.wizards.lengthsection;

import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Thomas Jung
 */
public class ShowResultLengthSectionViewHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext)event.getApplicationContext();

    final String commandName = HandlerUtils.getCommandName( event );
    final Shell shell = (Shell)context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    try
    {
      final IWorkbenchWindow window = (IWorkbenchWindow)context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );

      /* close old secondaryid views, that have been restored */
      final IWorkbenchPage page = window.getActivePage();
      closeAllViews( page );

      final IScenarioResultMeta resultModel = modelProvider.getModel( IScenarioResultMeta.class.getName() );

      // open wizard
      final SelectLengthSectionWizard selectLengthSectionWizard = new SelectLengthSectionWizard( resultModel, window );
      final WizardDialog2 dialog = new WizardDialog2( shell, selectLengthSectionWizard );
      dialog.setRememberSize( true );

      if( dialog.open() != Window.OK )
      {
        closeAllViews( page );
        openErrorView( page );
      }
      else
        closeEmptyView( page );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      StatusDialog.open( shell, e.getStatus(), commandName );
    }

    return null;
  }

  private void closeEmptyView( final IWorkbenchPage page )
  {
    final IViewReference[] viewReferences = page.getViewReferences();
    for( final IViewReference reference : viewReferences )
    {
      final String id = reference.getId();
      final String secondaryId = reference.getSecondaryId();
      if( FeatureTemplateView.ID.equals( id ) && StringUtils.isBlank( secondaryId ) )
        page.hideView( reference );
    }
  }

  private void closeAllViews( final IWorkbenchPage page )
  {
    final IViewReference[] viewReferences = page.getViewReferences();
    for( final IViewReference reference : viewReferences )
    {
      final String id = reference.getId();
      if( FeatureTemplateView.ID.equals( id ) )
        page.hideView( reference );
    }
  }

  private void openErrorView( final IWorkbenchPage page )
  {
    try
    {
      final URL url = getClass().getResource( "resources/lengthsectionError.gft" ); //$NON-NLS-1$

      final Featuretemplate template = GisTemplateHelper.loadGisFeatureTemplate( url, new NullProgressMonitor() );

      final FeatureTemplateView featureView = (FeatureTemplateView)page.showView( FeatureTemplateView.ID );
      /* fixme, does not work... */
      featureView.setTemplate( template, null, null, null, null );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }
}