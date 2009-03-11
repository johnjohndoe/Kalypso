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
package org.kalypso.ui.view.action;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardNode;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.internal.dialogs.WorkbenchWizardElement;
import org.eclipse.ui.internal.dialogs.WorkbenchWizardListSelectionPage;
import org.eclipse.ui.internal.dialogs.WorkbenchWizardNode;
import org.eclipse.ui.model.AdaptableList;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;

/**
 * @author Christoph Küpferle
 */
@SuppressWarnings("restriction")
public class KalypsoWizardSelectionPage extends WorkbenchWizardListSelectionPage
{
  protected IMapModellView m_outline;

  public KalypsoWizardSelectionPage( final IWorkbench aWorkbench, final IStructuredSelection selection, final AdaptableList wizardElts, final String message, final IMapModellView outlineview )
  {
    super( aWorkbench, selection, wizardElts, message, null );
    m_outline = outlineview;
  }

  /**
   * @see org.eclipse.ui.internal.dialogs.WorkbenchWizardListSelectionPage#createWizardNode(org.eclipse.ui.internal.dialogs.WorkbenchWizardElement)
   */
  @Override
  protected IWizardNode createWizardNode( final WorkbenchWizardElement element )
  {
    return new WorkbenchWizardNode( this, element )
    {
      /**
       * @see org.eclipse.ui.internal.dialogs.WorkbenchWizardNode#createWizard()
       */
      @Override
      public IWorkbenchWizard createWizard( ) throws CoreException
      {
        /* Find the right map modell */
        final IKalypsoLayerModell mapModell = findMapModell();

        final IKalypsoDataImportWizard newWizard = (IKalypsoDataImportWizard) element.createWizard();
        newWizard.setCommandTarget( m_outline );
        newWizard.setMapModel( mapModell );
        if( newWizard instanceof Wizard )
          ((Wizard) newWizard).setWindowTitle( KalypsoWizardSelectionPage.this.getWizard().getWindowTitle() );
        return newWizard;
      }

      private IKalypsoLayerModell findMapModell( )
      {
        final ISelection selection = m_outline.getSelection();
        if( selection instanceof StructuredSelection )
        {
          final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
          if( firstElement instanceof IKalypsoLayerModell )
            return (IKalypsoLayerModell) firstElement;

          if( firstElement instanceof IKalypsoTheme )
            return (IKalypsoLayerModell) ((IKalypsoTheme) firstElement).getMapModell();
        }

        /* Without valid selection, new themes go top-level */
        return (IKalypsoLayerModell) m_outline.getMapPanel().getMapModell();
      }
    };
  }
}