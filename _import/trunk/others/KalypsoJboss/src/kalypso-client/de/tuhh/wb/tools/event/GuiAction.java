package de.tuhh.wb.tools.event;

import javax.swing.event.EventListenerList;
import javax.swing.JButton;
import javax.swing.JMenuItem;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.EventListener;
import java.util.ArrayList;
import java.util.Iterator;

//import de.tuhh.wb.jm.Debug;
//import de.tuhh.wb.tools.prefs.I18n;

import de.tuhh.wb.tools.gui.GuiButton;
import de.tuhh.wb.tools.gui.GuiMenuItem;

public class GuiAction implements ActionListener {
	private String myActionCommand;
	private EventListenerList listeners = new EventListenerList();
	private ArrayList guiElementList = new ArrayList();
	private JMenuItem myMenuItem = null;

	public GuiAction(String actionCommand) {
		this.myActionCommand = actionCommand;
	}

	public String getActionCommand() {
		return myActionCommand;
	}

	public void addActionListener(ActionListener listener) {
		listeners.add(ActionListener.class, listener);
		checkComponents();
	}

	public void removeActionListener(ActionListener listener) {
		listeners.remove(ActionListener.class, listener);
		checkComponents();
	}

	public boolean hasListeners() {
		return listeners.getListenerCount() > 0;
	}

	public void registerGuiElement(GuiElement guiElement) {
		if (!guiElementList.contains(guiElement))
			guiElementList.add(guiElement);
		
	}

	public void unregisterGuiElement(GuiElement guiElement) {
		if (guiElementList.contains(guiElement))
			guiElementList.remove(guiElementList.indexOf(guiElement));
			}

	public JButton getJButton() {
		return new GuiButton(this);
	}

	public JMenuItem getJMenuItem() {
		//Debug.println("new GuiMenuItem");
		return new GuiMenuItem(this);
	}

	private synchronized void checkComponents() {
		// this method checks all registered GuiElements
		// unvisible(not registered) GuiElements will check their status when made visible
		Iterator it = guiElementList.iterator();
		while (it.hasNext()) {
			((GuiElement) it.next()).setEnabled(hasListeners());
		}
		/*System.out.println(
			"Number of registrated "
				+ getActionCommand()
				+ " "
				+ listeners.getListenerCount());*/
	}

	private synchronized void fireActionEvent(ActionEvent e) {
		EventListener[] list = listeners.getListeners(ActionListener.class);
		for (int i = 0; i < list.length; i++) {
			((ActionListener) list[i]).actionPerformed(e);
		}
	}

	public void actionPerformed(ActionEvent e) {
		fireActionEvent(e);
	}
}
