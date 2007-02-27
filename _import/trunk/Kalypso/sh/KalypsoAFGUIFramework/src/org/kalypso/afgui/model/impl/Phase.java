package org.kalypso.afgui.model.impl;

import java.util.List;

import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * @author Patrice Congo
 */
public class Phase extends Task implements IPhase
{
  public Phase( Resource resource )
  {
    super( resource );
  }

  /**
   * @see org.kalypso.afgui.model.IPhase#getTaskGroups()
   */
  public List<ITaskGroup> getTaskGroups( )
  {
    return Schema.getTaskGroups( resource );
  }
}
