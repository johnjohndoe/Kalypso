package org.kalypso.ui.internal.dialogs;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardNode;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.help.WorkbenchHelp;
import org.eclipse.ui.internal.IHelpContextIds;
import org.eclipse.ui.internal.IWorkbenchGraphicConstants;
import org.eclipse.ui.internal.WorkbenchImages;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.internal.dialogs.WorkbenchWizardElement;
import org.eclipse.ui.internal.dialogs.WorkbenchWizardListSelectionPage;
import org.eclipse.ui.internal.dialogs.WorkbenchWizardNode;
import org.eclipse.ui.model.AdaptableList;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.IKalypsoUIConstants;
import org.kalypso.ui.internal.registry.KalypsoWizardRegistryReader;

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
/**
 * This class extends the ImportWizard. This enables to call the import wizard
 * from any action. Entry point to kalypso import wizards.
 */
public class KalypsoImportWizard extends Wizard
{
  private GisMapOutlineViewer m_outlineviewer;
  private IStructuredSelection selection;
  private IWorkbench workbench;
  private KalypsoWizardSelectionPage m_selectionPage;

  /**
   * Returns the import wizards that are available for invocation.
   */
  public KalypsoImportWizard( GisMapOutlineViewer outlineviewer )
  {
    super();
    m_outlineviewer = outlineviewer;
    setWindowTitle("Kalypso Daten Import");
  }
  /**
   * Creates the wizard's pages lazily.
   */
  public void addPages() {
    addPage (new KalypsoWizardSelectionPage(this.workbench, this.selection, getAvailableImportWizards(), "Kalypso Import", m_outlineviewer));
  }


  /**
   * This method must be overwirtten to only get import wizards that are
   * declared in the org.kalypo.ui plugin. Further the WizardsRegistryReader
   * must be encapsuled in KalypsoWizardsRegistryReader to make shure only
   * wizards from org.kalypso.ui are read. And not as specified from the
   * Workbench ui -> see WizardsRegistryReader.
   */
  protected AdaptableList getAvailableImportWizards()
  {
    AdaptableList wizards = new KalypsoWizardRegistryReader( IKalypsoUIConstants.PL_IMPORT ).getWizards();
    return wizards;
  }

  public GisMapOutlineViewer getOutlineViewer()
  {
    return m_outlineviewer;
  }

  public boolean performFinish()
  {
    //((KalypsoWizardSelectionPage) getPages()[0]).saveWidgetValues();
    return true;
  }
  /**
   * Initializes the wizard.
   */
  public void init(IWorkbench aWorkbench, IStructuredSelection currentSelection) {
    this.workbench = aWorkbench;
    this.selection = currentSelection;

    setWindowTitle("Diese Daten k�nnen in die Karte eingef�gt werden"); //$NON-NLS-1$
    setDefaultPageImageDescriptor(
      WorkbenchImages.getImageDescriptor(IWorkbenchGraphicConstants.IMG_WIZBAN_IMPORT_WIZ));
    setNeedsProgressMonitor(true);
  }
}