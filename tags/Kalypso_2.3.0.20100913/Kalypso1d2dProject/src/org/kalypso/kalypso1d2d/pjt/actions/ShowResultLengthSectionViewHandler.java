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

import java.net.URL;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;
import org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Thomas Jung
 */
public class ShowResultLengthSectionViewHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

    try
    {
      final IScenarioResultMeta resultModel = modelProvider.getModel( IScenarioResultMeta.class );

      final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );

      // open wizard
      final SelectLengthSectionWizard selectLengthSectionWizard = new SelectLengthSectionWizard( resultModel );
      final WizardDialog2 wizardDialog2 = new WizardDialog2( shell, selectLengthSectionWizard );
      if( wizardDialog2.open() == Window.OK )
      {
        final FeatureTemplateView featureView = (FeatureTemplateView) window.getActivePage().showView( FeatureTemplateView.ID );

        final String gmlResultPath = selectLengthSectionWizard.getSelectedLGmlResultPath();
        if( gmlResultPath == null )
          return StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.kalypso1d2d.pjt.actions.ShowResultLengthSectionViewHandler.0") ); //$NON-NLS-1$

        final UIJob job = new UIJob( Messages.getString("org.kalypso.kalypso1d2d.pjt.actions.ShowResultLengthSectionViewHandler.1") ) //$NON-NLS-1$
        {
          @Override
          public IStatus runInUIThread( IProgressMonitor monitor )
          {
            try
            {
              // load template
              // load template from resource:
              final URL url = getClass().getResource( "resources/lengthsection.gft" ); //$NON-NLS-1$

              Featuretemplate template = GisTemplateHelper.loadGisFeatureTemplate( url, new NullProgressMonitor() );

              URL urlContext = ResourceUtilities.createURL( scenarioFolder );

              // root feature
              String featurePath = ""; //$NON-NLS-1$

              // path to gml, relative to context
              String href = gmlResultPath;

              final String linkType = "gml"; //$NON-NLS-1$

              // set template to view in ui thread, sepcify href, featurePath and linkType
              featureView.setTemplate( template, urlContext, featurePath, href, linkType );

              return Status.OK_STATUS;
            }
            catch( Throwable e )
            {
              return StatusUtilities.statusFromThrowable( e );
            }
          }
        };
        job.schedule();

        return Status.OK_STATUS;
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    return Status.CANCEL_STATUS;
  }
}
