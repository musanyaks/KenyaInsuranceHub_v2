// ============================================================
// Kenya Insurance Hub v2 - Custom JavaScript
// ============================================================

$(document).ready(function() {

  // Dark/Light Theme Toggle
  const currentTheme = localStorage.getItem('theme') || 'light';
  document.documentElement.setAttribute('data-theme', currentTheme);

  window.toggleTheme = function() {
    const theme = document.documentElement.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
    document.documentElement.setAttribute('data-theme', theme);
    localStorage.setItem('theme', theme);
    if (window.Shiny) {
      Shiny.setInputValue('theme_mode', theme, {priority: 'event'});
    }
  };

  // Auto-format KES inputs with commas
  $(document).on('input', '.kes-input', function() {
    let val = $(this).val().replace(/,/g, '');
    if (val && !isNaN(val)) {
      $(this).val(parseInt(val).toLocaleString('en-US'));
    }
  });

  // Remove commas before Shiny receives value
  $(document).on('shiny:inputchanged', function(event) {
    if ($(event.target).hasClass('kes-input')) {
      event.value = event.value.replace(/,/g, '');
    }
  });

  // Scroll Reveal Animation
  const observer = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (entry.isIntersecting) entry.target.classList.add('active');
    });
  }, { threshold: 0.1 });

  document.querySelectorAll('.reveal').forEach(el => observer.observe(el));

  // Counter Animation for KPI cards
  const animateCounters = () => {
    document.querySelectorAll('.counter-anim').forEach(counter => {
      const target = parseFloat(counter.getAttribute('data-target'));
      if (!target || isNaN(target)) return;
      const duration = 2000;
      const start = performance.now();

      const update = (now) => {
        const elapsed = now - start;
        const progress = Math.min(elapsed / duration, 1);
        const ease = 1 - Math.pow(1 - progress, 3);
        const current = Math.floor(ease * target);
        counter.textContent = current.toLocaleString();
        if (progress < 1) requestAnimationFrame(update);
        else counter.textContent = target.toLocaleString();
      };
      requestAnimationFrame(update);
    });
  };

  $(document).on('shiny:value', function() {
    setTimeout(animateCounters, 100);
  });

  setTimeout(animateCounters, 500);
});
