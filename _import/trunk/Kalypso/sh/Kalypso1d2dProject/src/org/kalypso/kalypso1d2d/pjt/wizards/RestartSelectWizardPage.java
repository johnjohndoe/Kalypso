package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;

public class RestartSelectWizardPage extends SelectResultWizardPage
{
  private final String m_initialSelection; // TODO: implement initial selection (previous restarts)

  public RestartSelectWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final String initialSelection, final ViewerFilter filter )
  {
    super( pageName, title, titleImage, filter, null );
    m_initialSelection = initialSelection;
  }

}

