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
package org.kalypso.model.km.internal.ui.kmupdate;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.km.internal.KMPlugin;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class KMUpdateHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );

    final ISelection selection = HandlerUtil.getCurrentSelectionChecked( event );

    final CommandableWorkspace workspace = findWorkspace( selection );

    final Feature[] initialSelection = findInitialSelection(selection);

    final KMUpdateWizard kmWizard = new KMUpdateWizard( workspace, initialSelection );
    final IDialogSettings dialogSettings = DialogSettingsUtils.getDialogSettings( KMPlugin.getDefault(), "kmUpdateAction" ); //$NON-NLS-1$
    kmWizard.setDialogSettings( dialogSettings );

    final WizardDialog2 dialog = new WizardDialog2( shell, kmWizard );
    dialog.setRememberSize( true );
    dialog.open();

    return null;
  }

  private CommandableWorkspace findWorkspace( final ISelection selection ) throws ExecutionException
  {
    if( !(selection instanceof IFeatureSelection) )
      throw new ExecutionException( "This handler only works on features" ); //$NON-NLS-1$

    final IFeatureSelection featureSelection = (IFeatureSelection) selection;
    final Feature focusFeature = featureSelection.getFocusedFeature();
    if( focusFeature != null )
      return findWorkspace( featureSelection, focusFeature );

    final Object firstElement = featureSelection.getFirstElement();
    if( firstElement instanceof Feature )
      return findWorkspace( featureSelection, (Feature) firstElement );

    throw new ExecutionException( "Failed to find workspace" ); //$NON-NLS-1$
  }

  private CommandableWorkspace findWorkspace( final IFeatureSelection featureSelection, final Feature focusFeature ) throws ExecutionException
  {
    final CommandableWorkspace workspace = featureSelection.getWorkspace( focusFeature );
    if( workspace == null )
      throw new ExecutionException( "Failed to find workspace" ); //$NON-NLS-1$

    final Feature rootFeature = workspace.getRootFeature();
    if( !(rootFeature instanceof NaModell) )
      throw new ExecutionException( "This handler only works with a NaModell" ); //$NON-NLS-1$

    return workspace;
  }

  private Feature[] findInitialSelection( final ISelection selection )
  {
    if( selection instanceof IFeatureSelection )
      return FeatureSelectionHelper.getFeatures( (IFeatureSelection) selection );

    if( selection instanceof IStructuredSelection )
    {
      final IStructuredSelection structured = (IStructuredSelection) selection;
      final Object[] array = structured.toArray();
      final Collection<Feature> features = new ArrayList<>();
      for( final Object object : array )
      {
        if( object instanceof Feature )
          features.add( (Feature) object );
      }
      return features.toArray( new Feature[features.size()] );
    }

    return new Feature[0];
  }

}
