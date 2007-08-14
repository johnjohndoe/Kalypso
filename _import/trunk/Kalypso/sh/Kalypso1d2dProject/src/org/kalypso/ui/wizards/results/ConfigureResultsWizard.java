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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IResultModelDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ResultDB;
import org.kalypso.ogc.gml.CascadingKalypsoTheme;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.IDialogWithResult;

/**
 * @author GernotBelger
 */
public class ConfigureResultsWizard extends Wizard implements IWorkbenchWizard, IDialogWithResult
{
  protected static final String THEME_PREFIX = "Ergebniskarte - ";

  private RestartSelectWizardPage m_restartSelectWizardPage;

  private AbstractMapPart m_part;

  private IStatus m_result;

  private IResultModelDescriptor[] m_existingResultDescriptors;

  private GisTemplateMapModell m_modell;

  private String[] m_existentResultMaps;

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    setWindowTitle( "Ergebniskarte konfigurieren" );

    final MapPanel panel = m_part == null ? null : (MapPanel) m_part.getAdapter( MapPanel.class );
    m_modell = (GisTemplateMapModell) (panel == null ? null : panel.getMapModell());
    if( m_modell == null )
      m_existingResultDescriptors = null;
    else
    {
      m_existentResultMaps = findExistentResultMaps( m_modell );
      m_existingResultDescriptors = findExistentResultDescriptors( m_existentResultMaps );
    }

    m_restartSelectWizardPage = new RestartSelectWizardPage( "resultSelectionPage", m_existingResultDescriptors );
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
    final IResultModelDescriptor[] selectedResults = m_restartSelectWizardPage.getSelectedResults();

    m_result = process( selectedResults, m_modell );

    ErrorDialog.openError( getShell(), getWindowTitle(), "Themen konnten nicht hinzugefügt werden.", m_result );

    return m_result.isOK();
  }

  private IStatus process( final IResultModelDescriptor[] selectedResults, final GisTemplateMapModell modell )
  {
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( "", selectedResults.length );

        try
        {
          /* Remember those themes which are to be removed. */
          final Set<String> hrefsToRemove = new HashSet<String>();
          Collections.addAll( hrefsToRemove, m_existentResultMaps );

          /* Command contains all changes to the map modell */
          final CompositeCommand command = new CompositeCommand( "Ergebniskarte konfigurieren" );

          for( final IResultModelDescriptor result : selectedResults )
          {
            final String gmt = result.getGmt();

            final ISimulationDescriptor simulationDescriptor = result.getParent();
            final String calcUnitName = simulationDescriptor == null ? "?" : simulationDescriptor.getName();
            final String themeLabel = THEME_PREFIX + calcUnitName + " -  " + result.getModelName();

            final String href = "../" + gmt;

            if( hrefsToRemove.contains( href ) )
            {
              // theme already there, do nothing, but do not remove it later
              hrefsToRemove.remove( href );
            }
            else
            {
              /* Add theme which is not present yet */

              final AddThemeCommand cmd = new AddThemeCommand( modell, themeLabel, "gmt", "", href );
              command.addCommand( cmd );
            }

            monitor.worked( 1 );
          }

          /* Remove all themes, which are existent but not choosen */
          final IKalypsoThemeVisitor visitor = new IKalypsoThemeVisitor()
          {
            public boolean visit( final IKalypsoTheme theme )
            {
              if( theme instanceof CascadingKalypsoTheme )
              {
                final CascadingKalypsoTheme cascTheme = (CascadingKalypsoTheme) theme;
                final String href = cascTheme.getMapViewRefUrl();
                if( hrefsToRemove.contains( href ) )
                  command.addCommand( new RemoveThemeCommand( modell, theme ) );
              }

              return false;
            }
          };
          modell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );

          m_part.postCommand( command, null );

          return Status.OK_STATUS;
        }
        // catch( final CoreException ce )
        // {
        // throw ce;
        // }
        catch( final Throwable e )
        {
          e.printStackTrace();

          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    return RunnableContextHelper.execute( getContainer(), true, false, operation );
  }

  private IResultModelDescriptor[] findExistentResultDescriptors( final String[] existentResultPathes )
  {
    final List<IResultModelDescriptor> descs = new ArrayList<IResultModelDescriptor>( existentResultPathes.length );

    final Collection<String> gmtPathes = new HashSet<String>( existentResultPathes.length );
    Collections.addAll( gmtPathes, existentResultPathes );

    final ResultDB resultDB = KalypsoModel1D2DPlugin.getDefault().getResultDB();
    final IFeatureWrapperCollection<ISimulationDescriptor> simulationDescriptors = resultDB.getSimulationDescriptors();
    for( final ISimulationDescriptor simulationDescriptor : simulationDescriptors )
    {
      final IFeatureWrapperCollection<IResultModelDescriptor> resultModel = simulationDescriptor.getResultModel();
      for( final IResultModelDescriptor resultModelDescriptor : resultModel )
      {
        final String gmt = resultModelDescriptor.getGmt();
        final String href = "../" + gmt;
        if( gmtPathes.contains( href ) )
          descs.add( resultModelDescriptor );
      }
    }

    return descs.toArray( new IResultModelDescriptor[descs.size()] );
  }

  private String[] findExistentResultMaps( final GisTemplateMapModell modell )
  {
    final IKalypsoThemePredicate predicate = new IKalypsoThemePredicate()
    {
      public boolean decide( final IKalypsoTheme theme )
      {
        return (theme instanceof CascadingKalypsoTheme) && ((CascadingKalypsoTheme) theme).getName().startsWith( THEME_PREFIX );
      }
    };

    final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( predicate );
    modell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    final IKalypsoTheme[] foundThemes = visitor.getFoundThemes();

    final String[] result = new String[foundThemes.length];
    for( int i = 0; i < foundThemes.length; i++ )
      result[i] = ((CascadingKalypsoTheme) foundThemes[i]).getMapViewRefUrl();

    return result;
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
    m_part = (AbstractMapPart) currentState.getVariable( ISources.ACTIVE_PART_NAME );
  }

  /**
   * @see de.renew.workflow.contexts.IDialogWithResult#getResult()
   */
  public Object getResult( )
  {
    return m_result;
  }

}
