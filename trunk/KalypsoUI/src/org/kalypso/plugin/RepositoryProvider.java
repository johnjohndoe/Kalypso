package org.kalypso.plugin;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Liefert die Liste der Zur Verfügung stehende Repository Definitionen.
 * 
 * @author schlienger
 */
public class RepositoryProvider implements IStructuredContentProvider
{
  private static RepositoryProvider m_instance = null;
  
  private RepositoryProvider()
  {
    // TODO: private lassen?
  }
  
  public static RepositoryProvider getInstance()
  {
    if( m_instance == null )
      m_instance = new RepositoryProvider();
    
    return m_instance;
  }

  public static String[] getAvailableIdentifiers()
  {
    // TODO: z.Z. fest codiert. Sollte aber aus eine Konf-Datei gelesen werden.
    
    return new String[] { "FILE", "KALYPSO_ZML_SERVICE" };
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    return getAvailableIdentifiers();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
    //
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
    //
  }
}
