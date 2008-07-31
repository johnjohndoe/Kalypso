package org.kalypso.model.wspm.sobek.result.processing.interfaces.implementation;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode.STRUCTURE_TYPE;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IRetardingBasinNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWorkspaceCache;

public class RetardingBasinNodeResultWrapper implements IRetardingBasinNodeResultWrapper
{

  private final IFolder m_resultFolder;

  private final IEmptyNode m_node;

  public RetardingBasinNodeResultWrapper( IEmptyNode node, IFolder resultFolder, IWorkspaceCache cache )
  {
    Assert.isTrue( STRUCTURE_TYPE.eRetardingBasin.equals( node.getStructureType() ) );

    m_node = node;
    m_resultFolder = resultFolder;
  }

  public void init( )
  {
    throw new NotImplementedException();

  }

}
