package de.renew.workflow.connector.cases;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;


/**
 * 
 * @author Stefan Kurzbach
 */
public class SimpleCaseDataProvider implements ICaseDataProvider<Object>
{

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#setCurrent(org.eclipse.core.resources.IFolder)
   */
  public void setCurrent( final IContainer caseDataFolder )
  {
    try
    {
      if( caseDataFolder != null )
        caseDataFolder.refreshLocal( IFolder.DEPTH_INFINITE, new NullProgressMonitor() );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }
  }

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#reloadModel()
   */
  public void reloadModel( )
  {

  }

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#getModel(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  public Object getModel( final Class modelClass )
  {
    return null;
  }

  public boolean isDirty( )
  {
    return false;
  }

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#isDirty(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  public boolean isDirty( final Class modelClass )
  {
    return false;
  }

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#saveModel(java.lang.Class,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @SuppressWarnings("unchecked")
  public void saveModel( final Class modelClass, IProgressMonitor monitor )
  {

  }

  /**
   * @see de.renew.workflow.cases.ICaseDataProvider#saveModel(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void saveModel( IProgressMonitor monitor )
  {

  }

}
