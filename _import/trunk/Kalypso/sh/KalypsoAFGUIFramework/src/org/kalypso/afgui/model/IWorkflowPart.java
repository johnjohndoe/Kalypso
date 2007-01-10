package org.kalypso.afgui.model;

public interface IWorkflowPart extends IWorkflowConcept {

	public String getURI();

	public String getName();

	public IHelp getHelp();

	public Object getModelObject();

}
