package org.kalypso.afgui.model.impl;

import java.util.List;

import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;

public class SubTaskGroup extends Task implements ISubTaskGroup
{
  public SubTaskGroup( Resource resource )
  {
    super( resource );
  }

  public List<ITask> getTasks( )
  {
    return Schema.getTasks( resource );
  }

  @Override
  public String toString( )
  {
    return getName();
  }
}
