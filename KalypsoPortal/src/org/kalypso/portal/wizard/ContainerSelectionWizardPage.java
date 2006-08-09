package org.kalypso.portal.wizard;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.internal.ide.misc.ContainerSelectionGroup;

public class ContainerSelectionWizardPage extends WizardPage
{

  ContainerSelectionGroup m_group;

  final IContainer m_root;

  private Listener m_listener = new Listener()
  {

    public void handleEvent( Event event )
    {
      if( event.type == SWT.Selection )
      {
        setPageComplete( false );
        IPath containerPath = m_group.getContainerFullPath();
        IResource resource = m_root.findMember( containerPath );
        if( resource instanceof IProject )
        {
          m_containerPath = (IProject) resource;
          setPageComplete( true );
        }
      }

    }
  };

  IProject m_containerPath;

  public ContainerSelectionWizardPage( String pageName, IContainer root )
  {
    super( pageName );
    m_root = root;
  }

  public ContainerSelectionWizardPage( String pageName, String title, ImageDescriptor titleImage, IContainer root )
  {
    super( pageName, title, titleImage );
    m_root = root;
  }

  public void createControl( Composite parent )
  {

    final Composite top = new Composite( parent, SWT.NONE );

    m_group = new ContainerSelectionGroup( top, m_listener, false, "Bitte ein Wizard auswählen...", true );
    m_group.pack();
    setControl( top );

  }

  public IProject getSelectedProject( )
  {
    return m_containerPath;
  }
}
