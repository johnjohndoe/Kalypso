package org.kalypso.ui.calcwizard.createpages;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ui.nature.ModelNature;

/**
 * Die Implementierung erzeugt einen völlig neuen Rechenfall im
 * Prognoseverzeichnis
 * 
 * @author belger
 */
public class AddNewCalcCaseChoice implements IAddCalcCaseChoice
{
  private Control m_control;

  private final String m_label;

  private final IProject m_project;

  private final AddCalcCasePage m_page;

  public AddNewCalcCaseChoice( final String label, final IProject project,
      final AddCalcCasePage page )
  {
    m_label = label;
    m_project = project;
    m_page = page;
    m_page.getClass();
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    final Label label = new Label( panel, SWT.NONE );
    label.setText( "keine weiteren Eingaben nötig" );
    final GridData labelData = new GridData();
    labelData.grabExcessHorizontalSpace = true;
    labelData.grabExcessVerticalSpace = true;
    labelData.horizontalAlignment = GridData.CENTER;
    labelData.verticalAlignment = GridData.CENTER;
    label.setLayoutData( labelData );

    m_control = panel;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#perform(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IFolder perform( final IProgressMonitor monitor ) throws CoreException
  {
    final ModelNature nature = (ModelNature)m_project.getNature( ModelNature.ID );
    return nature.createNewPrognose( monitor );
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#getControl()
   */
  public Control getControl()
  {
    return m_control;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#toString()
   */
  public String toString()
  {
    return m_label;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#update(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void update( final IProgressMonitor monitor )
  {
  // tut nix
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#isUpdateCalcCase()
   */
  public boolean isUpdateCalcCase()
  {
    return true;
  }
}