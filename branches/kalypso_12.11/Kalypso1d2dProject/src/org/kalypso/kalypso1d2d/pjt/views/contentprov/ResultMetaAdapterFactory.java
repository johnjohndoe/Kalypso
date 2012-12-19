package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.IWorkbenchAdapter2;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Stefan Kurzbach
 */
public class ResultMetaAdapterFactory implements IAdapterFactory
{
  private final ResultMetaWorkbenchAdapter m_resultMetaAdapter = new ResultMetaWorkbenchAdapter();

  @Override
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( adaptableObject instanceof IResultMeta )
    {
      return m_resultMetaAdapter;
    }

    return null;
  }

  @Override
  public Class< ? >[] getAdapterList( )
  {
    return new Class[] { IWorkbenchAdapter.class, IWorkbenchAdapter2.class };
  }
}