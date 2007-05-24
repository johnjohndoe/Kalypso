package de.renew.workflow.connector;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.model.WorkbenchContentProvider;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.context.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.SimpleCaseHandlingProjectNature;

public class CaseHandlingProjectContentProvider extends WorkbenchContentProvider
{
  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object element )
  {
    final Object[] children = super.getChildren( element );
    if( element instanceof IProject )
    {
      final IProject project = (IProject) element;
      try
      {
        final CaseHandlingProjectNature<Case> nature = (CaseHandlingProjectNature<Case>) project.getNature( SimpleCaseHandlingProjectNature.ID );
        if( nature != null )
        {
          final List<Object> resultList = new ArrayList<Object>(children.length + 3);
          resultList.addAll( Arrays.asList( children ) );          
          resultList.addAll(nature.getCaseManager().getCases());
          return resultList.toArray();
        }
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
    }
    return children;
  }
}
