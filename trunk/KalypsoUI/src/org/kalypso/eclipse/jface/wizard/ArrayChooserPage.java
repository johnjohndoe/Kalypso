package org.kalypso.eclipse.jface.wizard;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

/**
 * @author belger
 */
public class ArrayChooserPage extends WizardPage
{
  private final Object m_chooseables;
  private CheckboxTableViewer m_viewer;

  /**
   * @param chooseables
   *          Used as input for {@link ArrayContentProvider}
   */
  public ArrayChooserPage( final Object chooseables, final String pageName, final String title,
      final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

    m_chooseables = chooseables;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    m_viewer = CheckboxTableViewer.newCheckList( parent, SWT.BORDER );
    m_viewer.getTable().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_viewer.setLabelProvider( new LabelProvider() );
    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setInput( m_chooseables );
    
    setControl( m_viewer.getTable() );
  }
  
  public Object[] getChoosen()
  {
    return m_viewer.getCheckedElements();
  }
}