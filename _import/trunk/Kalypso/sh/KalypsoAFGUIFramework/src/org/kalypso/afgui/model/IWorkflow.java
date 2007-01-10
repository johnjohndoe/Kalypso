package org.kalypso.afgui.model;

import java.util.List;

public interface IWorkflow extends IWorkflowPart {
	public List<ITaskGroup> getTaskGroups();
}